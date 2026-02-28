#ifndef FUNCTIONS_H_INCLUDED
#define FUNCTIONS_H_INCLUDED

#include <vector>
#include <iostream>
#include <windows.h>
#include <mmsystem.h>
#include <graphics.h>
#include <winbgim.h>
#include "Model.h"

using namespace std;

// Function to display the matrix
void showMatrix(vector<vector<cell>> matrix);

// Function to draw the grid on the screen
void drawMatrixGrid(vector<vector<cell>> &matrix);

// Function to fill a cell with a given color
void fillCellWithColor(int i, int j, int color, vector<vector<cell>> &matrix);

//Function to highlight selected cell
void drawHighlight(vector<vector<cell>> &matrix, int i, int j, int highlightColor);
void removeHighlight(vector<vector<cell>> &matrix, int i, int j);
void handleCellClick(vector<vector<cell>> &matrix, int cellX, int cellY);
// Reset matrix to initial state
void resetMatrix(vector<vector<cell>> &matrix);
// Button for levels and generate solution
void drawButton(const string &buttonName, int X, int Y);
void drawMultipleButtons();
void playClickSound(int clikType);
// Logic of solution generation from current state of matrix
void onGenerateSolutionButtonPressed(vector<vector<cell>> &matrix);
void onRestartButtonPressed(vector<vector<cell>> &matrix);
void onGenerateButtonPressed(vector<vector<cell>> &matrix);
int getClickedButton(int mouseX, int mouseY);

//levels
void createLevelWithIndex(int index, vector<vector<cell>> &matrix);

#endif // FUNCTIONS_H_INCLUDED
