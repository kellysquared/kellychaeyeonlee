/**
 * @file maptiles.cpp
 * Code for the maptiles function.
 */

#include <iostream>
#include <map>
#include <unordered_map>
#include "maptiles.h"
using namespace std;
Point<3> convertToXYZ(LUVAPixel pixel) {
    return Point<3>( pixel.l, pixel.u, pixel.v );
}

MosaicCanvas* mapTiles(SourceImage const& theSource, vector<TileImage>& theTiles) {
    //make a vector 
    vector<Point<3>> tileColors;
    for (const TileImage& tile : theTiles) {
        LUVAPixel urmom = tile.getAverageColor();
        Point<3> xyzColor(urmom.l, urmom.u, urmom.v);
        tileColors.push_back(xyzColor);
    }
    std::map<Point<3>, TileImage*> colorToTileMap;

    for (size_t i = 0; i < theTiles.size(); ++i) {
        colorToTileMap[tileColors[i]] = &theTiles[i];
    }
    KDTree<3> colorKDTree(tileColors);

    MosaicCanvas* mosaic = new MosaicCanvas(theSource.getRows(), theSource.getColumns());

    // looopppp
    for (int i = 0; i < theSource.getRows(); ++i) {
        for (int j = 0; j < theSource.getColumns(); ++j) {
            LUVAPixel urdad = theSource.getRegionColor(i, j);
            Point<3> c(urdad.l, urdad.u, urdad.v);
            TileImage* near = colorToTileMap[colorKDTree.findNearestNeighbor(c)];

            mosaic->setTile(i, j, near);
        }
    }
    return mosaic;
}


