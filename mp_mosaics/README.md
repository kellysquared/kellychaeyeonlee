**Project Overview:**
This project involves creating a PhotoMosaic generator using KD-Trees to efficiently map tile images to regions of a source image based on color similarity. The goal is to develop a system that divides an image into smaller sections and replaces each section with a tile image that closely matches the color of that section.

**Objectives:**

- To learn about KD-Trees and their application in nearest neighbor search (NNS).
- To implement a KD-Tree data structure.
- To use the KD-Tree for mapping tile images to a source image to create a PhotoMosaic.

**Tools and Technologies Used:**

- C++
- Image Processing Libraries


### Project Components

**Code Files:**

- **`kdtree.hpp`**:
Defines the KD-Tree data structure, which is used for organizing points in a k-dimensional space. This is essential for efficient nearest neighbor searches in image processing tasks.
- **`sourceimage.cpp`**:
Handles operations related to the source image, including loading, processing, and manipulating the image data for further use in the project.
- **`mosaiccanvas.cpp`**:
Implements the creation of mosaic images by dividing a source image into smaller tiles and replacing them with matching images from a dataset.
- **`maptiles.cpp`**:
Contains functions for mapping tiles from a dataset to the appropriate sections of the source image to create a mosaic effect.
