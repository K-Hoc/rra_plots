from pathlib import Path

import albumentations as alb
import numpy as np
import pandas as pd
import tensorflow as tf
from keras.models import Sequential
from keras.layers import (
    RandomContrast,
    RandomRotation,
    RandomTranslation,
    RandomFlip,
    RandomBrightness,
    RandomContrast,
)


def augment(image, size):
    data = {"image": image}

    transforms = alb.Compose(
        [
            alb.RandomBrightnessContrast(p=0.2),
            alb.HorizontalFlip(p=0.5),
            alb.RandomBrightness(p=0.2),
            alb.RandomFog(0.3, 0.7, p=0.3),
            alb.HorizontalFlip(p=0.5),
            alb.RandomContrast(limit=0.2, p=0.3),
        ]
    )
    aug_data = transforms(**data)
    aug_img = aug_data["image"]
    aug_img = tf.cast(aug_img / 255.0, tf.float32)
    return tf.image.resize(aug_img, size=[size, size])


def augment_keras(image, label, seed, size):
    image, label = resize_and_rescale(image, label, size)
    image = tf.image.stateless_random_hue(image, 0.3, seed)
    image = tf.image.stateless_random_saturation(image, 0.5, 1.0, seed)
    image = tf.image.stateless_random_jpeg_quality(image, 77, 99, seed)
    image = tf.image.stateless_random_brightness(image, 0.2, seed)
    image = tf.image.stateless_random_contrast(image, 0.2, 0.5, seed)
    image = tf.image.stateless_random_flip_left_right(image, seed)
    return image, label


augmentation = Sequential(
    [
        # RandomRotation(factor=.15),
        RandomTranslation(height_factor=0.1, width_factor=0.1),
        RandomBrightness(0.5),
        RandomFlip("horizontal"),
        RandomContrast(factor=0.1),
        RandomBrightness(0.2),
        RandomContrast(factor=0.3),
    ]
)


@tf.function
def process_data(image, label, img_size):
    aug_img = tf.numpy_function(func=augment, inp=[image, img_size], Tout=tf.float32)
    return aug_img, label


@tf.function
def set_shapes(
    img: tf.Tensor, label: tf.Tensor, img_shape: (int, int, int) = (600, 600, 3)
):
    img = tf.ensure_shape(img, img_shape)
    label = tf.reshape(label, (10,))
    return img, label


def _lead_zero(word: str):
    wl = word.split("_")
    return "_".join([wl[0].zfill(2)] + wl[1:])


def photo_mapper(csv_file: Path | str, root: Path):
    if isinstance(root, Path):
        root = str(root.absolute())
    exclude = {"43_oak_l_12", "8_spruce_l_5", "17_spruce_d_11"}
    data = pd.read_csv(
        csv_file, usecols=lambda n: n.startswith("gc") or n == "uniqueID"
    )
    data = data[~data["uniqueID"].isin(exclude)]
    numeric = list(data.columns)
    numeric.remove("uniqueID")
    data[numeric] /= 100
    data["image_dir"] = (
        data["uniqueID"].str.split("_").str[:2].str.join("_").map(_lead_zero)
    )
    data["image_path"] = [
        "/".join([root, x, f"{y}_P.jpg"])
        for x, y in zip(data["image_dir"], data["uniqueID"])
    ]
    return data.drop(["image_dir", "uniqueID"], axis=1)


def filter_path_exists(df: pd.DataFrame):
    return df[[(Path(i)).exists() for i in df["image_path"]]]


def resize_and_rescale(image, label, size):
    image = tf.cast(image, tf.float32)
    image = tf.image.resize(image, [size, size])
    image = image / 255.0
    return image, label


@tf.function
def resize(image, label, size):
    image = tf.image.convert_image_dtype(image, tf.float32)
    return tf.image.resize(image, [size, size]), label


def split_dataset_by_substr(
    ds,
    split_by_col = "image_path",
    path_substr=None
):
    # Step 1: Create a function that checks if any substring matches
    def matches_substr(example, label):
        for example in ds.take(1):
            print(example)

        image_path = example[split_by_col]
        # Create list of boolean condition
        conditions = [tf.strings.contains(image_path, substring) for substring in path_substr]
        # Return True if any condiction is true
        return tf.reduce_any(tf.stack(conditions))
    
    # Step 2: Create the opposite condition
    def not_matches_substr(example, label):
        return tf.logical_not(matches_substr(example, label))
    
    # Step 3: Apply filters
    matched_ds = ds.filter(matches_substr)
    unmatched_ds = ds.filter(not_matches_substr)

    return matched_ds, unmatched_ds


def get_dataset_partitions(
    ds,
    ds_size,
    train_split=0.8,
    val_split=0.1,
    test_split=0.1,
    shuffle=True,
    shuffle_size=1000,
    seed=4,
):
    assert (train_split + test_split + val_split) == 1

    if shuffle:
        # Specify seed to always have the same split distribution between runs
        ds = ds.shuffle(shuffle_size, seed)

    train_size = int(train_split * ds_size)
    val_size = int(val_split * ds_size)

    train_ds = ds.take(train_size)
    val_ds = ds.skip(train_size).take(val_size)
    test_ds = ds.skip(train_size).skip(val_size)

    return train_ds, val_ds, test_ds    



def calc_bias(df, end=None, start=0):
    if end is None:
        end = len(df.columns)
    data = df.values[:, start:end].astype(float)
    # print("data shape: ", data.shape)
    label_freq = np.mean(data, 0)
    label_weights = 1 / (label_freq + 1e-8)
    label_weights /= np.sum(label_weights)
    return label_weights


@tf.function
def parse_image(filename):
    file = tf.io.read_file(filename)
    image = tf.image.decode_image(file)
    return image


def create_dataset(df: pd.DataFrame, target_str, num_calls: int):
    targets = [c for c in df.columns if c.startswith(target_str)]
    y = np.stack(df[targets].to_numpy()).astype(float)
    x = df["image_path"].to_numpy().astype(str)
    x_set = tf.data.Dataset.from_tensor_slices(x)
    # Honestly??!!!
    x_list = list(x_set.map(parse_image, num_parallel_calls=num_calls))
    return tf.data.Dataset.from_tensor_slices((x_list, y))
