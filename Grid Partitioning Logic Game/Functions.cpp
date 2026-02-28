#include "Functions.h"
#include "GameLogic.h"
lasthighlightedCell lasthighlightedCell;
gameState state;
vector<vector<cell>> initialMatrix(GRID_SIZE, vector<cell>(GRID_SIZE));


void printMatrix(const std::vector<std::vector<int>> &matrix)
{
    for (const auto &row : matrix)
    {
        for (int cell : row)
        {
            std::cout << std::setw(2) << cell << " ";
        }
        std::cout << "\n";
    }
    std::cout << "\n";
}

void playClickSound(int clickType)
{
    if (clickType == 0)
    {
        PlaySound(TEXT("click.wav"), NULL, SND_FILENAME | SND_ASYNC);
    }
    else if (clickType == 1)
    {
        PlaySound(TEXT("soft_click.wav"), NULL, SND_FILENAME | SND_ASYNC);
    }
    else if (clickType == 2)
    {
        PlaySound(TEXT("level_click.wav"), NULL, SND_FILENAME | SND_ASYNC);
    }
    else if (clickType == 3)
    {
        PlaySound(TEXT("game_win.wav"), NULL, SND_FILENAME | SND_ASYNC);
    }
}

void showMatrix(const vector<vector<cell>> matrix)
{
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            if (matrix[i][j].isFilled)
                cout << matrix[i][j].color << " ";
            else
                cout << "0 ";
        }
        cout << endl;
    }
    cout << endl;
}

void resetMatrix(vector<vector<cell>> &matrix)
{
    for (int i = 0; i < GRID_SIZE; i++)
        for (int j = 0; j < GRID_SIZE; j++)
        {
            matrix[i][j].isFilled = false;
            matrix[i][j].isHighlighted = false;
            matrix[i][j].color = BLACK;
            fillCellWithColor(i, j, BLACK, matrix);
        }
    removeHighlight(matrix, lasthighlightedCell.i, lasthighlightedCell.j);
    lasthighlightedCell.reset();
    state.reset();
}

//Call this function once on apear Life Cycle
void drawMatrixGrid(vector<vector<cell>> &matrix)
{
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            matrix[i][j].x = j * CELL_SIZE + CELL_SIZE;
            matrix[i][j].y = i * CELL_SIZE + CELL_SIZE;

            setcolor(WHITE);
            rectangle(matrix[i][j].x, matrix[i][j].y,
                      matrix[i][j].x + CELL_SIZE, matrix[i][j].y + CELL_SIZE);
            fillCellWithColor(i, j, matrix[i][j].color, matrix);
        }
    }
}

void fillCellWithColor(int i, int j, int color, vector<vector<cell>> &matrix)
{
    setfillstyle(SOLID_FILL, color);
    bar(matrix[i][j].x + 1, matrix[i][j].y + 1,
        matrix[i][j].x + CELL_SIZE - 1, matrix[i][j].y + CELL_SIZE - 1);
}

// Desenăm highlight-ul cu un border de alta culoare
void drawHighlight(vector<vector<cell>> &matrix, int highlightColor, int i, int j)
{
    // Setăm culoarea bordurii
    setcolor(highlightColor);
    // Desenăm mai multe rectangle suprapuse pentru un border mai gros
    int borderThickness = 6;
    for (int offset = 2; offset < borderThickness; ++offset)
    {
        rectangle(matrix[i][j].x + offset, matrix[i][j].y + offset,
                  matrix[i][j].x + CELL_SIZE - offset, matrix[i][j].y + CELL_SIZE - offset);
    }
    matrix[i][j].isHighlighted = true;
    if(!state.isZonePathStarted)
        lasthighlightedCell.color = matrix[i][j].color;
}

// Eliminăm highlight-ul
void removeHighlight(vector<vector<cell>> &matrix, int i, int j)
{
    if (i >= 0 && j >= 0 && i < GRID_SIZE && j < GRID_SIZE)
    {
        fillCellWithColor(i, j, matrix[i][j].color, matrix);
        setcolor(WHITE);
        rectangle(matrix[i][j].x, matrix[i][j].y,
                  matrix[i][j].x + CELL_SIZE, matrix[i][j].y + CELL_SIZE);

        // Resetează flag-ul de evidențiere
        matrix[i][j].isHighlighted = false;
    }
}

void showLosePopup(vector<vector<cell>> &matrix)
{
    // Dimensiunile ferestrei pop-up
    int popupWidth = 300, popupHeight = 150;
    int screenWidth = getmaxx(), screenHeight = getmaxy();
    int popupX = (screenWidth - popupWidth) / 2;
    int popupY = (screenHeight - popupHeight) / 2;

    // Desenăm pop-up-ul
    setfillstyle(SOLID_FILL, LIGHTGRAY);
    bar(popupX, popupY, popupX + popupWidth, popupY + popupHeight);

    // Conturul pop-up-ului
    setcolor(BLACK);
    rectangle(popupX, popupY, popupX + popupWidth, popupY + popupHeight);


    // Textul mesajului
    setbkcolor(LIGHTGRAY);
    setcolor(BLACK);
    settextstyle(SIMPLEX_FONT, HORIZ_DIR, 2);
    outtextxy(popupX + 20, popupY + 30, "Ai pierdut!");
    playClickSound(3);

    // Butonul "OK"
    int buttonWidth = 100, buttonHeight = 40;
    int buttonX = popupX + (popupWidth - buttonWidth) / 2;
    int buttonY = popupY + popupHeight - buttonHeight - 10;
    setfillstyle(SOLID_FILL, WHITE);
    bar(buttonX, buttonY, buttonX + buttonWidth, buttonY + buttonHeight);
    setcolor(BLACK);
    rectangle(buttonX, buttonY, buttonX + buttonWidth, buttonY + buttonHeight);
    outtextxy(buttonX + 25, buttonY + 10, "OK");

    // Așteptăm un click pe butonul "OK"
    while (true)
    {
        if (ismouseclick(WM_LBUTTONDOWN))
        {
            int mouseX, mouseY;
            getmouseclick(WM_LBUTTONDOWN, mouseX, mouseY);
            if (mouseX >= buttonX && mouseX <= buttonX + buttonWidth &&
                    mouseY >= buttonY && mouseY <= buttonY + buttonHeight)
            {
                // Ștergem pop-up-ul suprapunând fundalul
                setfillstyle(SOLID_FILL, BLACK); // Presupunem fundal negru
                bar(popupX, popupY, popupX + popupWidth, popupY + popupHeight);

                clearmouseclick(WM_LBUTTONDOWN); // Ștergem evenimentul de click
                drawMatrixGrid(matrix);
                onRestartButtonPressed(matrix);  // Resetăm nivelul
                return; // Ieșim din funcție
            }
        }
    }
}
void showWinPopup(vector<vector<cell>> &matrix)
{
    state.isGameEnd = true; // Prevenim orice interacțiune ulterioară
    // Dimensiunile ferestrei pop-up
     if (lasthighlightedCell.i != -1 && lasthighlightedCell.j != -1)
    {
        removeHighlight(matrix, lasthighlightedCell.i, lasthighlightedCell.j);
        lasthighlightedCell.reset();
    }
    int popupWidth = 300, popupHeight = 150;
    int screenWidth = getmaxx(), screenHeight = getmaxy();
    int popupX = (screenWidth - popupWidth) / 2;
    int popupY = (screenHeight - popupHeight) / 2;

    // Desenăm pop-up-ul
    setfillstyle(SOLID_FILL, LIGHTGRAY);
    bar(popupX, popupY, popupX + popupWidth, popupY + popupHeight);

    // Conturul pop-up-ului
    setcolor(BLACK);
    rectangle(popupX, popupY, popupX + popupWidth, popupY + popupHeight);


    // Textul mesajului
    setbkcolor(LIGHTGRAY);
    setcolor(BLACK);
    settextstyle(SIMPLEX_FONT, HORIZ_DIR, 2);
    outtextxy(popupX + 20, popupY + 30, "Ai castigat!");
    playClickSound(3);

    // Butonul "OK"
    int buttonWidth = 100, buttonHeight = 40;
    int buttonX = popupX + (popupWidth - buttonWidth) / 2;
    int buttonY = popupY + popupHeight - buttonHeight - 10;
    setfillstyle(SOLID_FILL, WHITE);
    bar(buttonX, buttonY, buttonX + buttonWidth, buttonY + buttonHeight);
    setcolor(BLACK);
    rectangle(buttonX, buttonY, buttonX + buttonWidth, buttonY + buttonHeight);
    outtextxy(buttonX + 25, buttonY + 10, "OK");

    // Așteptăm un click pe butonul "OK"
    while (true)
    {
        if (ismouseclick(WM_LBUTTONDOWN))
        {
            int mouseX, mouseY;
            getmouseclick(WM_LBUTTONDOWN, mouseX, mouseY);
            if (mouseX >= buttonX && mouseX <= buttonX + buttonWidth &&
                    mouseY >= buttonY && mouseY <= buttonY + buttonHeight)
            {
                // Ștergem pop-up-ul suprapunând fundalul
                setfillstyle(SOLID_FILL, BLACK); // Presupunem fundal negru
                bar(popupX, popupY, popupX + popupWidth, popupY + popupHeight);
                clearmouseclick(WM_LBUTTONDOWN); // Ștergem evenimentul de click
                drawMatrixGrid(matrix);
                resetMatrix(matrix); // Resetăm nivelul
                onRestartButtonPressed(matrix);
                state.isGameEnd = false;
                return; // Ieșim din funcție
            }
        }
    }
}
bool checkForWin(vector<vector<cell>> &matrix)
{
    bool ok = true;
    for (auto &color : state.zoneColors)
    {
        if (!color.second)
        {
            ok = false;
            cout << "Zona de culoare: " << lasthighlightedCell.color << " este completata dar au ramas alte culori de completat \n";
            break;
        }
    }

    return ok;
}



// Draw a single button at a given position with a specific name
void drawButton(const string &buttonName, int X, int Y)
{
    // Create a mutable copy of buttonName
    char label[buttonName.length() + 1];
    strcpy(label, buttonName.c_str());

    // Draw button background
    setfillstyle(SOLID_FILL, LIGHTGRAY);
    bar(X, Y, X + BUTTON_WIDTH, Y + BUTTON_HEIGHT);

    // Draw button border
    setcolor(BLACK);
    rectangle(X, Y, X + BUTTON_WIDTH, Y + BUTTON_HEIGHT);

    // Draw button label
    setbkcolor(LIGHTGRAY);
    setcolor(BLACK);
    outtextxy(X + 10, Y + 10, label);
}


// Draw multiple buttons in a vertical arrangement
void drawMultipleButtons()
{
    const int numberOfButtons = 11;

    for (int i = 0; i < numberOfButtons; i++)
    {
        int buttonY = BUTTON_Y + i * (BUTTON_HEIGHT + BUTTON_SPACING); // Calculate Y position for each button
        string label;
        switch(i)
        {
        case 0:
            label = "Get Solution";
            break;

        case 9:
            label = "Generate";
            break;
        case 10:
            label = "Restart";
            break;
        default:
            label = "Level "+ to_string(i);
            break;
        }
        drawButton(label, BUTTON_X, buttonY);
    }
}

// Direcțiile de deplasare (stânga, dreapta, sus, jos)
int directions[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

// Verifică dacă o poziție este validă (în matrice)
bool isValid(int x, int y,vector<vector<cell>> &matrix)
{
    return x >= 0 && x < GRID_SIZE && y >= 0 && y < GRID_SIZE && !matrix[x][y].isFilled;
}

void onRestartButtonPressed(vector<vector<cell>> &matrix)
{
    resetMatrix(matrix);
    // Resetează matricea curentă la configurarea inițială
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            matrix[i][j] = initialMatrix[i][j];
            fillCellWithColor(i, j, matrix[i][j].color, matrix);
        }
    }

    cout << "Nivelul a fost resetat la configuratia initiala!" << endl;
}

// Generare nivel aleatoriu
void onGenerateButtonPressed(vector<vector<cell>> &matrix)
{
    cout << "S-a generat un nivel nou" << endl;

    // Resetează matricea înainte de a genera un nivel nou
    resetMatrix(matrix);

    // Inițializează generatorul de numere aleatoare (o singură dată pe durata rulării programului)
    static bool initialized = false;
    if (!initialized)
    {
        srand(static_cast<unsigned int>(time(0)));
        initialized = true;
    }

    // Lista de culori (de exemplu, culori între 1 și 7)
    vector<int> colors;
    for (int i = 1; i <= GRID_SIZE; ++i)
        colors.push_back(i);

    // Amestecă culorile pentru o distribuție aleatorie
    random_shuffle(colors.begin(), colors.end());

    // Lista de coordonate ale matricei
    vector<pair<int, int>> coordinates;
    for (int i = 0; i < GRID_SIZE; ++i)
    {
        for (int j = 0; j < GRID_SIZE ; ++j)
        {
            coordinates.emplace_back(i, j);
        }
    }

    // Amestecă pozițiile pentru a decide aleator pozițiile celulelor colorate
    random_shuffle(coordinates.begin(), coordinates.end());

    // Atribuie culorile către pozițiile amestecate
    for (size_t cnt = 0; cnt < colors.size(); ++cnt)
    {
        int i = coordinates[cnt].first;
        int j = coordinates[cnt].second;
        int color = colors[cnt];

        // Marchează celula în matrice
        matrix[i][j].isFilled = true;
        matrix[i][j].color = color;
        fillCellWithColor(i, j, color, matrix);
    }
    initialMatrix = matrix; // Salvează matricea inițială
}

// Function to call when button is pressed
void onGenerateSolutionButtonPressed(vector<vector<cell>>& matrix)
{
    cout << "Incepe generarea solutiei\n";

    // Create and properly size the matrix copy
    vector<vector<int>> matrixCopy(GRID_SIZE, vector<int>(GRID_SIZE));

    // Convert your cell matrix to int matrix
    for(int i = 0; i < GRID_SIZE; i++)
    {
        for(int j = 0; j < GRID_SIZE; j++)
        {
            // Convert your colors to zone numbers (assuming BLACK is your empty state)
            matrixCopy[i][j] = (matrix[i][j].color == BLACK) ? 0 : matrix[i][j].color;
        }
    }

    // Grupați toate pozițiile pe fiecare număr
    map<int, vector<Position>> freq;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            int index = matrixCopy[i][j];
            if (index)
            {
                freq[index].push_back(Position(i, j));
            }
        }
    }

    // Calculăm toate soluțiile pentru fiecare număr
    map<int, vector<vector<Position>>> allPaths;
    vector<int> keys;
    for (const auto &pair : freq)
    {
        int targetNumber = pair.first;
        vector<Position> startZone = pair.second;
        MatrixPathFinder finder(matrixCopy, GRID_SIZE, startZone, targetNumber);
        vector<vector<Position>> paths = finder.findAllPaths();
        allPaths[targetNumber] = paths;
        keys.push_back(targetNumber);
    }


    // Inițializăm matricea și soluția finală
    vector<vector<int>> matrixSolution(GRID_SIZE, vector<int>(GRID_SIZE, 0));
    vector<vector<Position>> finalSolution;

    // Găsim prima soluție completă
    if (findFirstCompleteSolution(matrixSolution, allPaths, keys, 0, finalSolution))
    {
        vector<vector<int>> outputMatrix(matrixSolution.size(), vector<int>(matrixSolution[0].size(), 0));
        cout << "Solutie gasita!:\n";
        for (int i = 0; i < keys.size(); i++)
        {
            cout << "Pentru zona numarul(" << keys[i] << ") : ";
            for (const auto &pos : finalSolution[i])
            {
                matrix[pos.line][pos.col].color = keys[i];
                fillCellWithColor(pos.line, pos.col, keys[i], matrix);
                cout << "(" << pos.line << "," << pos.col << ") ";
            }
            cout << "\n";
        }
        printSolution(outputMatrix);
        playClickSound(3);
        showWinPopup(matrix);
    }
    else
    {
        showLosePopup(matrix);
        cout << "Nu exista solutie pentru aceasta configuratie\n";
    }
}
void fillLevelCells(vector<pair<int, int>> cells, vector<vector<cell>> &matrix)
{
    // Iterăm prin celule și aplicăm culorile
    for (int cnt = 0; cnt < GRID_SIZE; ++cnt)
    {
        int i = cells[cnt].first;
        int j =  cells[cnt].second;
        // Destructurează perechea
        const int color = cnt + 1;          // Culoarea bazată pe index

        // Marchează celula curentă ca fiind umplută și setează culoarea
        matrix[i][j].isFilled = true;
        matrix[i][j].color = color;

        // Completează alte celule pe baza acestei celule
        fillCellWithColor(i, j, color, matrix);
    }

    // Afișează matricea rezultată
    showMatrix(matrix);
    initialMatrix = matrix;
}
void createLevelWithIndex(int index,vector<vector<cell>> &matrix)
{
    // Resetează matricea
    resetMatrix(matrix);

    vector<vector<pair<int, int>>> levels =
    {
        {{0, 0}, {1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}, {6, 6}, {7, 7}}, // Nivel 1
        {{2, 0}, {6, 1}, {5, 2}, {6, 2}, {7, 2}, {7, 3}, {6, 5}, {6, 7}},
        {{0, 2}, {1, 0}, {1, 1}, {2, 1}, {2, 2}, {3, 3}, {2, 5}, {6, 7}},
        {{3, 5}, {4, 3}, {5, 4}, {5, 6}, {6, 4}, {6, 6}, {6, 7}, {7, 7}},
        {{2, 0}, {2, 2}, {6, 2}, {6, 3}, {6, 5}, {7, 0}, {7, 2}, {7, 3}},
        {{0, 7}, {1, 7}, {4, 0}, {4, 2}, {5, 0}, {5, 4}, {5, 5}, {5, 6}},
        {{1, 1}, {2, 7}, {3, 0}, {5, 4}, {5, 7}, {6, 0}, {6, 7}, {7, 2}},
        {{0, 4}, {2, 5}, {2, 6}, {3, 1}, {3, 2}, {3, 7}, {6, 4}, {7, 3}}, // Nivel 8
    };

    switch(index)
    {

    case 1:
        fillLevelCells(levels[0], matrix);
        break;
    case 2:
        fillLevelCells(levels[1], matrix);
        break;
    case 3:
        fillLevelCells(levels[2], matrix);
        break;
    case 4:
        fillLevelCells(levels[3], matrix);
        break;
    case 5:
        fillLevelCells(levels[4], matrix);
        break;
    case 6:
        fillLevelCells(levels[5], matrix);
        break;
    case 7:
        fillLevelCells(levels[6], matrix);
        break;
    case 8:
        fillLevelCells(levels[7], matrix);
        break;

    default:
        break;
    }

}

void checkZone(vector<vector<cell>> &matrix,int i, int j)
{
    // Dacă celula curentă este neagră și o celulă evidențiată există
    if (lasthighlightedCell.color != BLACK && matrix[i][j].color == BLACK)
    {

        // Verificăm dacă culoarea actuală este completă
        bool isColorUsed = false;
        for (const auto &zone : state.zoneColors)
        {
            if (zone.first == lasthighlightedCell.color && zone.second)
            {
                isColorUsed = true;
                break;
            }
        }

        // Dacă culoarea este completă, nu mai putem folosi această culoare
        if (isColorUsed)
        {
            cout << "Zona cu culoare: "<<lasthighlightedCell.color <<" este deja completata \n";
            return;
        }
        // Verificăm dacă vecinii au aceeași culoare ca și ultima celulă evidențiată

        // Daca ma aflu in interiorul matricei
        if ((j > 0 && matrix[i][j - 1].color == lasthighlightedCell.color) || // Verificăm stânga
                (j < GRID_SIZE - 1 && matrix[i][j + 1].color == lasthighlightedCell.color) || // Verificăm dreapta
                (i > 0 && matrix[i - 1][j].color == lasthighlightedCell.color) || // Verificăm sus
                (i < GRID_SIZE - 1 && matrix[i + 1][j].color == lasthighlightedCell.color)) // verific sus

        {
            if (state.zonePathLenght == GRID_SIZE - 1)
            {
                // Finalizăm zona
                state.isZonePathStarted = false;
                state.zonePathLenght = 1;

                // Marcam culoarea ca fiind completă
                for (auto &zone : state.zoneColors)
                {
                    if (zone.first == lasthighlightedCell.color)
                    {
                        zone.second = true;
                        break;
                    }
                }
                if (checkForWin(matrix)) // manual win
                {
                    // Umplem celula curentă
                    fillCellWithColor(i, j, lasthighlightedCell.color, matrix);
                    matrix[i][j].color = lasthighlightedCell.color;
                    matrix[i][j].isFilled = true;
                    showWinPopup(matrix);
                    cout << "Ai castigat!\n";
                    playClickSound(3);


                }
            }
            else
            {
                // Continuăm zona
                state.isZonePathStarted = true;
                state.zonePathLenght++;
            }


            // Dacă zona este completă (lungimea atinge 8)
            if (state.zonePathLenght == 8)
            {
                cout << "Zona completata!\n";
                state.isZonePathStarted = false; // Permitem începerea unei noi zone
                state.zonePathLenght = 1;
            }

            // Umplem celula curentă
            fillCellWithColor(i, j, lasthighlightedCell.color, matrix);
            matrix[i][j].color = lasthighlightedCell.color;
            matrix[i][j].isFilled = true;
        }

        else
            cout << "Celula selectata nu este valida pentru continuarea zonei!" << endl;

    }
}

int getClickedButton(int mouseX, int mouseY)
{
    const int numberOfButtons = 11;

    for (int i = 0; i < numberOfButtons; i++)
    {
        int y = BUTTON_Y + i * (BUTTON_HEIGHT + BUTTON_SPACING);
        if (mouseX >= BUTTON_X && mouseX <= BUTTON_X + BUTTON_WIDTH &&
                mouseY >= y && mouseY <= y + BUTTON_HEIGHT)
        {
            return i + 1;
        }
    }

    return -1; // No button clicked
}

// Funcția principală pentru click pe o celulă
void handleCellClick(vector<vector<cell>> &matrix, int cellX, int cellY)
{
    if (cellX >= 0 && cellX < GRID_SIZE && cellY >= 0 && cellY < GRID_SIZE)
    {
        if (state.isGameEnd) return;
        int i = cellY;
        int j = cellX;

        // Eliminăm highlight-ul anterior
        if (lasthighlightedCell.i != -1 && lasthighlightedCell.j != -1)
        {
            removeHighlight(matrix, lasthighlightedCell.i, lasthighlightedCell.j);
        }


        checkZone(matrix, i, j);
        // Desenăm highlight-ul pentru noua celulă
        drawHighlight(matrix, YELLOW, i, j);

        // Actualizăm ultima celulă evidențiată
        lasthighlightedCell.i = i;
        lasthighlightedCell.j = j;

        cout << "Lungimea zonei curente: "<<state.zonePathLenght << " Culoarea zonei: "<<lasthighlightedCell.color<< endl;
        showMatrix(matrix); // print matrix in console
    }
}






