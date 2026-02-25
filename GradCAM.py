# Script for GradCAM application
# Imports
from pathlib import Path
import tensorflow as tf
import numpy as np
import cv2
import matplotlib.pyplot as plt
import argparse
import os

from tensorflow.keras.applications.xception import preprocess_input
from tensorflow.keras.preprocessing import image
from model_zoo import model_dict

# Argument parser
parser = argparse.ArgumentParser()
parser.add_argument(
    "-m",
    "--model",
    help="Model to choose from model zoo",
    default="XCEPTION"
)
parser.add_argument(
    "-o",
    "--output",
    help="root, where the model is safed",
    required=True
)
parser.add_argument(
    "-gc",
    "--groundcover",
    help="True for groundcover model, False for complexity",
    default=False,
    type=bool
)
opts = parser.parse_args()

model_selector = opts.model.upper()
output = opts.output
gc = opts.groundcover

RESCALE = model_dict[model_selector.upper()][1]

## Definitions
def grad_cam(model, img_array, layer_name):
    """
    Computes Grad-CAM for a given image and model.

    Parameters:
    - model: Trained Xception-based model
    - img_array: Preprocessed image (4D tensor)
    - layer_name: Last convolutional layer name

    Returns:
    - heatmap: Grad-CAM activation map
    """
    
    # Get the last convolutional layer
    conv_layer = model.get_layer(layer_name)

    # Create a new model that outputs both conv layer activations and final predictions
    grad_model = tf.keras.models.Model(
        inputs=[model.input], 
        outputs=[conv_layer.output, model.output]
    )

    # Compute the gradient of the predicted class w.r.t. feature maps
    with tf.GradientTape() as tape:
        conv_output, preds = grad_model(img_array)  # Forward pass
        pred_index = tf.argmax(preds, axis=-1)  # Get class index with highest probability -> Maximum value along specified axis
        pred_class_score = tf.gather(preds, pred_index, axis=-1, batch_dims=1)
    
    # Compute gradients
    grads = tape.gradient(pred_class_score, conv_output)

    # Compute pooled gradients (global average pooling over spatial dimensions)
    pooled_grads = tf.reduce_mean(grads, axis=(0, 1, 2))

    # Weight feature maps using the pooled gradients
    conv_output = conv_output[0]
    conv_output = conv_output * pooled_grads

    # Generate the heatmap
    heatmap = np.mean(conv_output, axis=-1)
    heatmap = np.maximum(heatmap, 0)  # Apply ReLU
    heatmap /= np.max(heatmap)  # Normalize

    return heatmap

def preprocess_img(img_path):
    img = image.load_img(img_path, target_size=(RESCALE, RESCALE))
    img = image.img_to_array(img)
    img = np.expand_dims(img, axis=0)
    img = preprocess_input(img)  # Normalize for Xception
    return img

def overlay_heatmap(img_path, heatmap, alpha=0.4):
    img = cv2.imread(img_path)
    
    if img is None:
        raise ValueError("Image could not be loaded. Check if the file path is correct!")

    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)  # Convert BGR to RGB
    
    if heatmap is None or not isinstance(heatmap, np.ndarray) or heatmap.size == 0:
        raise ValueError("Heatmap is Empty or None!")
    
    if len(heatmap.shape) < 2:
        raise ValueError(f"Invalid heatmap shape: {heatmap.shape}")

    heatmap = np.nan_to_num(heatmap, nan=0.0, posinf=1.0, neginf=0.0) # Remove NaN/Inf
    heatmap = heatmap.astype(np.float32)

    # Resize to match image
    heatmap = cv2.resize(heatmap, (img.shape[1], img.shape[0]), interpolation=cv2.INTER_LINEAR)

    heatmap = np.uint8(255 * heatmap)  # Convert to 8-bit
    heatmap = cv2.applyColorMap(heatmap, cv2.COLORMAP_JET)  # Apply colormap

    superimposed_img = cv2.addWeighted(img, 1 - alpha, heatmap, alpha, 0)

    return superimposed_img

### Program code
# Loading model
model = tf.keras.models.load_model(f"{output}/model")
model = model.layers[0]
model.summary()

with open("test_images", "r") as src:
    if gc == False:
        lines = [e for e in src.read().splitlines() if e[-5] != 'P']
    else:
        lines = [e for e in src.read().splitlines() if e[-5] == 'P']

img_paths = tf.data.Dataset.from_tensor_slices(np.array(lines).astype(str))

for img_path in img_paths:
    print(img_path)
    # Convert tensor to string
    img_path = img_path.numpy().decode("utf-8")
    print(img_path)

    # Load image and preprocess
    img_array = preprocess_img(img_path)

    # Generate Grad-CAM heatmap
    heatmap = grad_cam(model, img_array, layer_name='block14_sepconv2_act')

    # Overlay on image & save
    sup_img = overlay_heatmap(img_path, heatmap)

    outdir = Path(output).joinpath('heatmaps')
    print(outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    tf.keras.utils.save_img(
        path=outdir.joinpath(f"{os.path.basename(img_path)}"),
        x=sup_img
    )
