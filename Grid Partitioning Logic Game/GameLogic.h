#ifndef GAMELOGIC_H_INCLUDED
#define GAMELOGIC_H_INCLUDED

#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include <set>
#include <unordered_set>
#include <algorithm>
#include <chrono>  
#include <iomanip> 
using namespace std;


struct Position
{
    int line, col;
    Position(int l, int c) : line(l), col(c) {}
    bool operator==(const Position &other) const
    {
        return line == other.line && col == other.col;
    }
    bool operator<(const Position &other) const
    {
        if (line != other.line)
            return line < other.line;
        return col < other.col;
    }
};


bool isValidSolution(vector<vector<int>> &matrix, const vector<Position> &path);
void undoSolution(vector<vector<int>> &matrix, const vector<Position> &path);
bool findFirstCompleteSolution(
    vector<vector<int>> &matrix,
    map<int, vector<vector<Position>>> &allPaths,
    vector<int> &keys,
    int currentIndex,
    vector<vector<Position>> &finalSolution);
void printSolution(vector<vector<int>> solution);

class MatrixPathFinder
{
private:
    vector<vector<int>> matrix;
    int N;
    int targetNumber;
    vector<Position> startZone;
    set<set<Position>> uniqueSolutions; // Folosim set de set pentru solutii unice
    set<Position> currentPath;

    // Directiile de deplasare: sus, jos, stanga, dreapta
    const int dx[4] = {-1, 1, 0, 0};
    const int dy[4] = {0, 0, -1, 1};

    bool isValid(const Position &pos);


    // Verifica dacă o pozitie este adiacenta cu path-ul curent
    bool isAdjacent(const Position &pos);

    void backtrack();

public:
    MatrixPathFinder(std::vector<std::vector<int>> &mat, int size, std::vector<Position> &startingZone, int target);

    vector<vector<Position>> findAllPaths();

};


#endif // GAMELOGIC_H_INCLUDED
