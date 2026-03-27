from keras.models import load_model
import pandas as pd
from sys import argv
from model_zoo import model_dict
import tensorflow as tf
import numpy as np
from pathlib import Path
import argparse

# # Imports for GradCAM
# from pytorch_grad_cam import GradCAM
# from pytorch_grad_cam.utils.model_targets import ClassifierOutputTarget
# from pytorch_grad_cam.utils.image import show_cam_on_image
# import cv2

# _, model_selector, out = argv

parser = argparse.ArgumentParser()
parser.add_argument("-m","--model", help="Model to choose from model zoo", required=True)
parser.add_argument("-o","--output", help="output root, where model is safed", required=True)
parser.add_argument("-gc","--groundcover", help="True when model is used for estimating ground cover, False when used for complexity", type=bool, default=False)
opts = parser.parse_args()

model_selector = opts.model.upper()
out = opts.output
gc = opts.groundcover
print("gc: ", gc)

RESCALE = model_dict[model_selector.upper()][1]
AUTO = tf.data.AUTOTUNE


def preprocess(img):
    img = tf.io.read_file(img)
    img = tf.image.decode_jpeg(img, channels=3)
    img = tf.image.resize(img, (RESCALE, RESCALE))
    print("Processed image shape: ", img.shape)
    return img / 255.0


with open("matched_images.txt", "r") as src:
    if gc == False:
        lines = [e for e in src.read().splitlines() if e[-5] != 'P']
    else:
        lines = [e for e in src.read().splitlines() if e[-5] == 'P']


#cont = input(f"Using {len(lines)} images. Continue? ")
#if cont == 'n':
#    exit(1)


image_paths = tf.data.Dataset.from_tensor_slices(np.array(lines).astype(str))
image_dataset = image_paths.map(preprocess).batch(16)
print("Number of batches in dataset: ", len(image_dataset))

model = load_model(f'{out}/model')
print("Model: ", model.summary())

print("Starting predictions...")

predictions = model.predict(image_dataset)
print("Shape of predictions array: ", predictions.shape)
print(predictions[:5])

print('\033[32m[DONE]\033[0m')

# print("Starting GradCAM...")
# target_layers = model.get_layer("xception")
# input_tensor = image_dataset # Create an input tensor image for your model..
# # Note: input_tensor can be a batch tensor with several images!
# 
# cam = GradCAM(model=model, target_layers=target_layers)
# # You can also pass aug_smooth=True and eigen_smooth=True, to apply smoothing.
# grayscale_cam = cam(input_tensor=input_tensor, targets=None)
# 
# grayscale_cam = grayscale_cam[0, :]
# visualization = show_cam_on_image(image_dataset, grayscale_cam, use_rgb=True)
# 
# cv2.imwrite("./output/", visualization)

outdir = Path(out)
outdir.mkdir(exist_ok=True)

headers = ""
if gc :
    headers = ['image_path'] + 'gc_Mature_Trees,gc_rejuvenation,gc_shrub_layer,gc_mosses,gc_ferns,gc_herb_layer,gc_grasses,gc_soil/foliage,gc_rock,gc_deadwood/stumps'.split(',')
    print("groundcover")
else:
    headers = ['image_path'] + 'grade_stand_density,grade_treespecies,grade_shrubs,grade_herbs,grade_grass,grade_moss,grade_deadwood,grade_layers,grade_mixing'.split(',') # for complexity
    print("complexity")

# headers = ['image_path'] + 'gc_Mature_Trees,gc_rejuvenation,gc_shrub_layer,gc_mosses,gc_ferns,gc_herb_layer,gc_grasses,gc_soil/foliage,gc_rock,gc_deadwood/stumps'.split(',')
results = pd.DataFrame(predictions)
paths = pd.DataFrame(lines)

print("results: ", results)

results = pd.concat([paths, results], axis=1)
results.columns = headers

results.to_csv(outdir.joinpath('predictions.csv'), index=False)
