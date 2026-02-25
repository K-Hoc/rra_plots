#import os
#os.environ["TF_USE_LEGACY_KERAS"] = "1"
# import tf_keras as keras # KH
import keras.applications
from keras.metrics import KLDivergence, MeanRelativeError, MeanSquaredError

model_dict = {
    "B0": (keras.applications.EfficientNetB0, 224),
    "B2": (keras.applications.EfficientNetB2, 260),
    "B4": (keras.applications.EfficientNetB4, 380),
    "B5": (keras.applications.EfficientNetB5, 456),
    "B7": (keras.applications.EfficientNetB7, 600),
    "XCEPTION": (keras.applications.Xception, 299),
    # ConvNeXt resolutions for all are 224 or 380, exept Base with 446
    "CNT" : (keras.applications.ConvNeXtTiny, 224),
    "CNS" : (keras.applications.ConvNeXtSmall, 380),
    "CNB" : (keras.applications.ConvNeXtBase, 446),
    "CNL" : (keras.applications.ConvNeXtLarge, 224),
    "CNXL": (keras.applications.ConvNeXtXLarge, 224)
}


def metrics(num_classes):
    return [KLDivergence(), MeanRelativeError([1.0] * num_classes), MeanSquaredError()]
