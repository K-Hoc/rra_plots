import os
import re

def save_matching_images(
    root_dir,
    output_file="image_list.txt",
    exclude_dirs=None,
    regex_pattern=r"^\d+_[a-zA-Z]+_c_\d+_[A-Z]\.(jpg|jpeg|png|tif|tiff|JPG|JPEG)$",
    save_full_path=True,
):
    if exclude_dirs is None:
        exclude_dirs = []

    # Compile regex for performance
    pattern = re.compile(regex_pattern)

    matches = []

    for dirpath, dirnames, filenames in os.walk(root_dir):
        # Remove excluded and hidden directories
        dirnames[:] = [
            d for d in dirnames 
            if not d.startswith(".") and d not in exclude_dirs
        ]

        for filename in filenames:
            # Skip hidden files
            if filename.startswith("."):
                continue

            if pattern.match(filename):
                file_path = os.path.join(dirpath, filename)
                matches.append(file_path if save_full_path else filename)

    # Save results
    with open(output_file, "w", encoding="utf-8") as f:
        for item in matches:
            f.write(item + "\n")

    print(f"✅ Saved {len(matches)} matching files to: {output_file}")


# ------ Example usage ------
if __name__ == "__main__":
    save_matching_images(
        root_dir="/data/public/projects/RapidAssessment/03_dataRaw/data/GoPro/lowRes/",
        output_file="matched_images.txt",
        exclude_dirs=["128Gb_goPro"],  # Optional
        regex_pattern=r"^\d+_[a-zA-Z]+_[a-zA-Z]_\d+_[A-Z]\.(jpg|JPG)$",  # Adjust here
        save_full_path=True,  # Set False if you only want filenames
    )