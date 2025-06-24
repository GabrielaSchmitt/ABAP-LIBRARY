from PIL import Image
from google.colab import files

# Step 1: Ask user to choose mode
mode = input("Enter output mode ('bw' for black & white, 'rgb' for color): ").strip().lower()
assert mode in ['bw', 'rgb'], "Invalid mode. Please choose 'bw' or 'rgb'."

# Step 2: Upload PNG file
uploaded = files.upload()

# Step 3: Process the uploaded file
for filename in uploaded.keys():
    # Open the image
    img = Image.open(filename)

    # Resize to 711 x 336
    img = img.resize((711, 336), resample=Image.Resampling.LANCZOS)

    if mode == 'bw':
        # Convert to grayscale
        img = img.convert("L")

        # Threshold to black and white (1-bit)
        threshold = 128
        img = img.point(lambda x: 255 if x > threshold else 0, mode='1')

        bmp_filename = filename.rsplit('.', 1)[0] + '_711x336_300dpi_bw.bmp'

    else:  # RGB mode
        # Ensure RGB (remove alpha if needed)
        if img.mode in ("RGBA", "P", "LA"):
            img = img.convert("RGB")

        bmp_filename = filename.rsplit('.', 1)[0] + '_711x336_300dpi_rgb.bmp'

    # Save as BMP with 300 DPI
    img.save(bmp_filename, format='BMP', dpi=(300, 300))

    print(f"Saved BMP file: {bmp_filename}")
    files.download(bmp_filename)
