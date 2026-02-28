#include <iostream>
#include <graphics.h>
#include <winbgim.h>
#include <vector>
#include "Functions.h"
#include "Model.h"
#include <windows.h>
#include "GameLogic.h"
using namespace std;



int main()
{
    vector<vector<cell>> matrix(GRID_SIZE, vector<cell>(GRID_SIZE));

    // Initialize graphics window
    int gd = DETECT, gm;
    initgraph(&gd, &gm, (char *)"");


    drawMatrixGrid(matrix);

    // Draw multiple buttons
    drawMultipleButtons();

    while (true)
    {
        // Wait for mouse click event
        if (ismouseclick(WM_LBUTTONDOWN))
        {
            // get mouse click coordonate
            int x, y;
            getmouseclick(WM_LBUTTONDOWN, x, y);

            // Debug the click coordinates
            cout << "Mouse clicked at: (" << x << ", " << y << ")" << endl;


            int clickedButton = getClickedButton(x, y); // if i click on button from right panel i get index of clicked from top to bottom

            // Check if mouse click coordonate is in matrix grid area
            if (x <= CELL_SIZE * (GRID_SIZE + 1) && x >= CELL_SIZE &&
                    y <= CELL_SIZE * (GRID_SIZE + 1) && y >= CELL_SIZE)
            {
                playClickSound(1);
                // Transform from coordinates (x, y) to matrix position (i, j; line, column) format
                int cellX = (x - CELL_SIZE) / CELL_SIZE;
                int cellY = (y - CELL_SIZE) / CELL_SIZE;
                handleCellClick(matrix, cellX, cellY);
            }
            else if(clickedButton != -1)
            {
                playClickSound(2);
                switch (clickedButton)
                {
                case 1:
                    onGenerateSolutionButtonPressed(matrix);
                    break;
                case 2:
                    createLevelWithIndex(1, matrix);
                    break;
                case 3:
                    createLevelWithIndex(2, matrix);
                    break;
                case 4:
                    createLevelWithIndex(3, matrix);
                    break;
                case 5:
                    createLevelWithIndex(4, matrix);
                    break;
                case 6:
                    createLevelWithIndex(5, matrix);
                    break;
                case 7:
                    createLevelWithIndex(6, matrix);
                    break;
                case 8:
                    createLevelWithIndex(7, matrix);
                    break;
                case 9:
                    createLevelWithIndex(8, matrix);
                    break;
                case 10:
                    onGenerateButtonPressed(matrix);
                    break;
                case 11: 
                    onRestartButtonPressed(matrix);
                    break;
                default:
                    break;
                }
            }
            else
            {
                playClickSound(0);
            }
        }
    }

    closegraph();
    return 0;
}
