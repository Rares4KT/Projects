#ifndef MODEL_H_INCLUDED
#define MODEL_H_INCLUDED

#include <vector>
#include <graphics.h>
#include <winbgim.h>
#define GRID_SIZE 8
#define CELL_SIZE 50

// Constants for button
#define BUTTON_X 50 * GRID_SIZE + 70
#define BUTTON_Y 35
#define BUTTON_WIDTH 100
#define BUTTON_HEIGHT 30
#define BUTTON_SPACING 10



struct cell
{
    bool isHighlighted = false; // The last focused cell, which was clicked
    bool isFilled = false;  // Whether the cell is filled
    int color = BLACK;      // Color of the cell
    int x, y;               // Top-left coordinates of the cell
};

struct lasthighlightedCell
{
    int i = -1;
    int j = -1;
    int color = BLACK; // black

    void reset()
    {
        i = -1;
        j = -1;
        color = BLACK;
    }
};


struct gameState
{
    int zonePathLenght = 1;
    bool isZonePathStarted = false;
    bool isGameEnd = false;
    std::vector<std::pair<int, bool>> zoneColors;


    // Constructor
    gameState()
    {
        for (int i = 1; i <= GRID_SIZE; i++)
        {
            zoneColors.push_back({i, false});
        }
    }

    // Reset method
    void reset()
    {
        zonePathLenght = 1;
        isZonePathStarted = false;
        isGameEnd = false;
        zoneColors.clear(); // Clear the vector
        for (int i = 1; i <= GRID_SIZE; i++)
        {
            zoneColors.push_back({i, false}); // Reinitialize the vector
        }
    }
};


#endif // MODEL_H_INCLUDED
