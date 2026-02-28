#include "GameLogic.h"

bool isValidSolution(vector<vector<int>> &matrix, const vector<Position> &path)
{
    for (const auto &pos : path)
    {
        if (matrix[pos.line][pos.col] != 0)
        {
            return false; // Celula este deja ocupata de alt numar
        }
    }

    // Alocam temporar aceasta solutie matricei
    for (const auto &pos : path)
    {
        matrix[pos.line][pos.col] = -1; // Marcam celulele ca ocupate pentru validare
    }

    return true;
}
void undoSolution(vector<vector<int>> &matrix, const vector<Position> &path)
{
    for (const auto &pos : path)
    {
        matrix[pos.line][pos.col] = 0; // Resetam celulele ocupate de aceasta solutie
    }
}
bool findFirstCompleteSolution(
    vector<vector<int>> &matrix,
    map<int, vector<vector<Position>>> &allPaths,
    vector<int> &keys,
    int currentIndex,
    vector<vector<Position>> &finalSolution)
{
    if (currentIndex == keys.size()) // Am procesat toate numerele
    {
        return true;
    }

    int number = keys[currentIndex];
    for (const auto &path : allPaths[number])
    {
        if (isValidSolution(matrix, path))
        {
            finalSolution.push_back(path); // Alegem solutia curenta
            // Marcam celulele cu numarul actual
            for (const auto &pos : path)
            {
                matrix[pos.line][pos.col] = number;
            }

            if (findFirstCompleteSolution(matrix, allPaths, keys, currentIndex + 1, finalSolution))
            {
                return true;
            }

            // Revert 
            finalSolution.pop_back();
            undoSolution(matrix, path);
        }
    }

    return false;
}
void printSolution(vector<vector<int>> solution)
{
    for (auto line : solution)
    {
        for (auto column : line)
        {
            cout << column << " ";
        }
        cout << endl;
    }
}

// Constructorul pentru MatrixPathFinder
MatrixPathFinder::MatrixPathFinder(vector<vector<int>> &mat, int size, vector<Position> &startingZone, int target)
    : matrix(mat), N(size), startZone(startingZone), targetNumber(target) {}

// Verifica daca pozitia este valida
bool MatrixPathFinder::isValid(const Position &pos)
{
    if (pos.line < 0 || pos.line >= N || pos.col < 0 || pos.col >= N)
        return false;

    if (matrix[pos.line][pos.col] != 0 && matrix[pos.line][pos.col] != targetNumber)
        return false;

    if (currentPath.find(pos) != currentPath.end())
        return false;

    return true;
}

// Verifica daca pozitia este adiacenta
bool MatrixPathFinder::isAdjacent(const Position &pos)
{
    for (const Position &currentPos : currentPath)
    {
        for (int i = 0; i < 4; i++)
        {
            int newX = currentPos.line + dx[i];
            int newY = currentPos.col + dy[i];
            if (pos.line == newX && pos.col == newY)
                return true;
        }
    }
    return false;
}

// Functia de backtracking
void MatrixPathFinder::backtrack()
{
    if (currentPath.size() == N)
    {
        uniqueSolutions.insert(currentPath);
        return;
    }

    set<Position> tried;
    for (int i = 0; i < N; i++)
    {
        for (int j = 0; j < N; j++)
        {
            Position pos(i, j);
            if (isValid(pos) && isAdjacent(pos) && tried.find(pos) == tried.end())
            {
                tried.insert(pos);
                currentPath.insert(pos);
                backtrack();
                currentPath.erase(pos);
            }
        }
    }
}

// Gaseste toate caile
vector<vector<Position>> MatrixPathFinder::findAllPaths()
{
    uniqueSolutions.clear();
    currentPath.clear();

    for (const Position &pos : startZone)
    {
        currentPath.insert(pos);
    }

    backtrack();

    vector<vector<Position>> result;
    for (const auto &solution : uniqueSolutions)
    {
        result.push_back(vector<Position>(solution.begin(), solution.end()));
    }

    return result;
}
