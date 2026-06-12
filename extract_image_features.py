import argparse
import os

import cv2
import numpy as np
import pandas as pd
from scipy.stats import entropy as _sp_entropy
# skimage moved some feature functions between versions; import robustly
try:
    from skimage.feature import greycomatrix, greycoprops, local_binary_pattern
except Exception:
    try:
        from skimage.feature.texture import greycomatrix, greycoprops
    except Exception:
        greycomatrix = greycoprops = None
    try:
        from skimage.feature import local_binary_pattern
    except Exception:
        try:
            from skimage.feature._lbp import local_binary_pattern
        except Exception:
            local_binary_pattern = None

    # progress bar (optional)
    try:
        from tqdm import tqdm
    except Exception:
        def tqdm(iterable, **kwargs):
            return iterable


def resolve_image_path(image_path: str, image_root: str | None = None) -> str:
    if os.path.isabs(image_path):
        return image_path
    if image_root:
        return os.path.join(image_root, image_path)
    return image_path


def load_image(image_path: str) -> np.ndarray | None:
    image = cv2.imread(image_path, cv2.IMREAD_COLOR)
    if image is None:
        return None
    return cv2.cvtColor(image, cv2.COLOR_BGR2RGB)


def compute_image_features(image: np.ndarray) -> dict:
    # Prepare representations
    img_f = image.astype(np.float32)
    h, w = img_f.shape[:2]
    img_uint8 = np.clip(img_f, 0, 255).astype(np.uint8)

    # Color statistics (RGB)
    mean_rgb = img_f.mean(axis=(0, 1))
    std_rgb = img_f.std(axis=(0, 1))
    total = mean_rgb.sum()
    green_ratio = float(mean_rgb[1] / total) if total > 0 else 0.0
    green_excess = float(2 * mean_rgb[1] - mean_rgb[0] - mean_rgb[2])
    vegetation_index = float(
        (2 * mean_rgb[1] - mean_rgb[0] - mean_rgb[2]) / (2 * mean_rgb[1] + mean_rgb[0] + mean_rgb[2])
    ) if (2 * mean_rgb[1] + mean_rgb[0] + mean_rgb[2]) > 0 else 0.0
    brightness_mean = float(img_f.mean())

    # HSV and LAB statistics
    hsv = cv2.cvtColor(img_uint8, cv2.COLOR_RGB2HSV).astype(np.float32)
    lab = cv2.cvtColor(img_uint8, cv2.COLOR_RGB2LAB).astype(np.float32)
    mean_hsv = hsv.mean(axis=(0, 1))
    std_hsv = hsv.std(axis=(0, 1))
    mean_lab = lab.mean(axis=(0, 1))
    std_lab = lab.std(axis=(0, 1))

    # Per-channel histograms (RGB) and entropy
    bins = 16
    channel_hist_entropy = {}
    for i, ch in enumerate(["R", "G", "B"]):
        hist, _ = np.histogram(img_uint8[:, :, i], bins=bins, range=(0, 255))
        hist = hist.astype(np.float32)
        if hist.sum() > 0:
            hist = hist / hist.sum()
        channel_hist_entropy[f"{ch}_hist_entropy"] = float(_sp_entropy(hist + 1e-12, base=2))

    # Grayscale statistics and entropy
    gray = cv2.cvtColor(img_uint8, cv2.COLOR_RGB2GRAY)
    gray_f = gray.astype(np.float32)
    gray_mean = float(gray_f.mean())
    gray_std = float(gray_f.std())
    hist_gray, _ = np.histogram(gray, bins=256, range=(0, 255))
    hist_gray = hist_gray.astype(np.float32)
    if hist_gray.sum() > 0:
        hist_gray = hist_gray / hist_gray.sum()
    img_entropy = float(_sp_entropy(hist_gray + 1e-12, base=2))

    # GLCM texture features (co-occurrence) using reduced gray levels
    try:
        levels = 8
        gl_gray = (gray_f * (levels - 1) / 255.0).astype(np.uint8)
        distances = [1]
        angles = [0, np.pi / 4, np.pi / 2, 3 * np.pi / 4]
        glcm = greycomatrix(gl_gray, distances=distances, angles=angles, levels=levels, symmetric=True, normed=True)
        glcm_props = {
            "contrast": float(greycoprops(glcm, "contrast").mean()),
            "dissimilarity": float(greycoprops(glcm, "dissimilarity").mean()),
            "homogeneity": float(greycoprops(glcm, "homogeneity").mean()),
            "energy": float(greycoprops(glcm, "energy").mean()),
            "correlation": float(greycoprops(glcm, "correlation").mean()),
        }
    except Exception:
        glcm_props = {k: 0.0 for k in ["contrast", "dissimilarity", "homogeneity", "energy", "correlation"]}

    # Local Binary Pattern (LBP) histogram
    try:
        P = 8
        R = 1
        lbp = local_binary_pattern(gray, P, R, method="uniform")
        lbp_hist, _ = np.histogram(lbp.ravel(), bins=np.arange(0, P + 3), range=(0, P + 2))
        lbp_hist = lbp_hist.astype(np.float32)
        if lbp_hist.sum() > 0:
            lbp_hist = lbp_hist / lbp_hist.sum()
        lbp_entropy = float(_sp_entropy(lbp_hist + 1e-12, base=2))
    except Exception:
        lbp_entropy = 0.0

    # Edge and contour / shape features
    edges = cv2.Canny(gray, 100, 200)
    edge_density = float(np.count_nonzero(edges) / (h * w))

    # Contours from Otsu threshold
    try:
        _, th = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
        contours, _ = cv2.findContours(th, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    except Exception:
        contours = []

    contour_count = int(len(contours))
    contour_areas = [float(cv2.contourArea(c)) for c in contours] if contours else []
    if contour_areas:
        max_area = float(max(contour_areas))
        mean_contour_area = float(np.mean(contour_areas))
        std_contour_area = float(np.std(contour_areas))
        largest_area_fraction = max_area / (h * w)
    else:
        max_area = 0.0
        mean_contour_area = 0.0
        std_contour_area = 0.0
        largest_area_fraction = 0.0

    perimeters = [float(cv2.arcLength(c, True)) for c in contours] if contours else []
    mean_perimeter = float(np.mean(perimeters)) if perimeters else 0.0

    # Hu moments (log-transformed) based on binary image
    try:
        moments = cv2.moments(th)
        hu = cv2.HuMoments(moments).flatten()
        hu_log = [float(-np.sign(h) * np.log10(abs(h) + 1e-12)) for h in hu]
    except Exception:
        hu_log = [0.0] * 7

    # Assemble feature dictionary
    features = {
        "mean_R": float(mean_rgb[0]),
        "mean_G": float(mean_rgb[1]),
        "mean_B": float(mean_rgb[2]),
        "std_R": float(std_rgb[0]),
        "std_G": float(std_rgb[1]),
        "std_B": float(std_rgb[2]),
        "brightness_mean": brightness_mean,
        "green_ratio": green_ratio,
        "green_excess": green_excess,
        "vegetation_index": vegetation_index,

        "mean_H": float(mean_hsv[0]),
        "mean_S": float(mean_hsv[1]),
        "mean_V": float(mean_hsv[2]),
        "std_H": float(std_hsv[0]),
        "std_S": float(std_hsv[1]),
        "std_V": float(std_hsv[2]),

        "mean_L": float(mean_lab[0]),
        "mean_A": float(mean_lab[1]),
        "mean_Blab": float(mean_lab[2]),
        "std_L": float(std_lab[0]),
        "std_A": float(std_lab[1]),
        "std_Blab": float(std_lab[2]),

        "gray_mean": gray_mean,
        "gray_std": gray_std,
        "img_entropy": img_entropy,
        "lbp_entropy": lbp_entropy,
        "edge_density": edge_density,

        "contour_count": contour_count,
        "max_contour_area": max_area,
        "mean_contour_area": mean_contour_area,
        "std_contour_area": std_contour_area,
        "largest_area_fraction": largest_area_fraction,
        "mean_perimeter": mean_perimeter,
    }

    # add GLCM props
    features.update(glcm_props)

    # add histogram entropy features
    features.update(channel_hist_entropy)

    # add Hu moments
    for i, v in enumerate(hu_log, start=1):
        features[f"hu_moment_{i}"] = v

    return features


def extract_features(
    input_csv: str,
    output_csv: str,
    path_column: str = "image_path",
    image_root: str | None = None,
    skip_missing: bool = False,
    quiet: bool = False,
):
    if input_csv.lower().endswith((".txt", ".list")):
        with open(input_csv, "r", encoding="utf-8") as handle:
            image_paths = [line.strip() for line in handle if line.strip()]
        df = pd.DataFrame({path_column: image_paths})
    else:
        df = pd.read_csv(input_csv)
        if path_column not in df.columns:
            if df.shape[1] == 1:
                df = df.rename(columns={df.columns[0]: path_column})
            else:
                raise ValueError(
                    f"Expected image path column '{path_column}' not found in input CSV. "
                    f"Available columns: {', '.join(df.columns)}"
                )

    results = []
    missing_count = 0
    invalid_count = 0

    for idx, row in df.iterrows():
        image_path = str(row[path_column])
        resolved_path = resolve_image_path(image_path, image_root)

        if not os.path.exists(resolved_path):
            missing_count += 1
            if skip_missing:
                if not quiet:
                    print(f"⚠️  Missing image skipped: {resolved_path}")
                continue
            raise FileNotFoundError(f"Image not found: {resolved_path}")

        image = load_image(resolved_path)
        if image is None:
            invalid_count += 1
            if skip_missing:
                if not quiet:
                    print(f"⚠️  Invalid image skipped: {resolved_path}")
                continue
            raise ValueError(f"Unable to read image: {resolved_path}")

        features = compute_image_features(image)
        features[path_column] = image_path
        results.append(features)

        if not quiet and (idx + 1) % 100 == 0:
            print(f"Processed {idx + 1} images...")

    feature_df = pd.DataFrame(results)
    feature_df.to_csv(output_csv, index=False)

    if not quiet:
        print(f"✅ Extracted features for {len(feature_df)} images")
        if missing_count:
            print(f"⚠️  Missing image paths: {missing_count}")
        if invalid_count:
            print(f"⚠️  Invalid images: {invalid_count}")
        print(f"Saved features to: {output_csv}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Extract per-image color and greenness features from images listed in a CSV file."
    )
    parser.add_argument(
        "--input-csv",
        required=True,
        help="Path to the input CSV or image list file containing image paths.",
    )
    parser.add_argument(
        "--output-csv",
        default="image_features.csv",
        help="Path to the output CSV file that will receive extracted features.",
    )
    parser.add_argument(
        "--path-column",
        default="image_path",
        help="Name of the column in the input CSV that contains image paths.",
    )
    parser.add_argument(
        "--image-root",
        default=None,
        help="Optional root directory that will be prepended to relative image paths.",
    )
    parser.add_argument(
        "--skip-missing",
        action="store_true",
        help="Continue processing if image files are missing or unreadable.",
    )
    parser.add_argument(
        "--quiet",
        action="store_true",
        help="Suppress progress output and only show final status.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    extract_features(
        input_csv=args.input_csv,
        output_csv=args.output_csv,
        path_column=args.path_column,
        image_root=args.image_root,
        skip_missing=args.skip_missing,
        quiet=args.quiet,
    )


if __name__ == "__main__":
    main()
