% Gives access to the game menu
play :-
    main_menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Menu and Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Displays the game menu and allows the players to configure game type, board size and AI difficulty levels
main_menu :-
    write('Welcome to Doblin!'), nl,
    write('Choose grid size (e.g., 6 for 6x6): '),
    read(Size),
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('Choose an option: '),
    read(Choice),
    configure_game(Choice, Size, GameConfig),
    initial_state(GameConfig, GameState),
    game_loop(GameState).

% Configures the game based on the menu selection.
configure_game(1, Size, config(human, human, _Level1, _Level2)) :-
write('Human vs Human selected.'), nl.

configure_game(2, config(human, computer, Level, _Level2)) :-
write('Human vs Computer selected.'), nl,
write('Choose computer difficulty (1: Easy, 2: Hard): '),
read(Level).

configure_game(3, config(computer, computer, Level1, Level2)) :-
write('Computer vs Computer selected.'), nl,
write('Choose difficulty for Computer 1 (1: Easy, 2: Hard): '),
read(Level1),
write('Choose difficulty for Computer 2 (1: Easy, 2: Hard): '),
read(Level2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Initialization and State
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initializes the game state based on the configuration
initial_state(config(Player1, Player2, Level1, Level2, Size), game_state(Grid1, Grid2, Player1, Player2, RowMapping, ColMapping)) :-
    initialize_grids(Size, Grid1, Grid2, RowMapping, ColMapping).

% Initializes two grids and their coordinate mappings.
initialize_grids(Size, Grid1, Grid2, RowMapping, ColMapping) :-
    create_empty_grid(Size, Grid1),
    create_empty_grid(Size, Grid2),
    generate_mappings(Size, RowMapping, ColMapping).

% Creates an empty grid of the specified size.
create_empty_grid(Size, Grid) :-
    length(Grid, Size),
    maplist(length_(Size), Grid).

length_(Size, List) :-
    length(List, Size).

% Generates row and column mappings for the second grid.
generate_mappings(Size, RowMapping, ColMapping) :-
    numlist(1, Size, Base),
    random_permutation(Base, RowMapping),
    random_permutation(Base, ColMapping).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Displays the current game state, including both grids and the current player.
display_game(game_state(Grid1, Grid2, CurrentPlayer, _, _, _)) :-
    write('Grid 1:'), nl,
    print_board(Grid1),
    write('Grid 2:'), nl,
    print_board(Grid2),
    format('Current Player: ~w~n', [CurrentPlayer]).

% Prints the game board.
print_board(Board) :-
    maplist(print_row, Board).

print_row(Row) :-
    maplist(write, Row), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move Execution and Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Executes a move if valid and updates the game state.
move(game_state(Grid1, Grid2, Player1, Player2, RowMapping, ColMapping), Move, game_state(NewGrid1, NewGrid2, Player2, Player1, RowMapping, ColMapping)) :-
    validate_move(Grid1, Move),
    place_symbol(Grid1, Grid2, Move.row, Move.col, Player1, RowMapping, ColMapping, NewGrid1, NewGrid2).

% Places a symbol on both grids according to the mappings.
place_symbol(Grid1, Grid2, Row, Col, Symbol, RowMapping, ColMapping, NewGrid1, NewGrid2) :-
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

% Updates a specific cell in a grid.
update_grid(Grid, Row, Col, Symbol, NewGrid) :-
    nth1(Row, Grid, OldRow),
    nth1(Col, OldRow, _, TempRow),
    nth1(Col, NewRow, Symbol, TempRow),
    nth1(Row, Grid, _, TempGrid),
    nth1(Row, NewGrid, NewRow, TempGrid).

% Translates coordinates for the second grid.
translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol) :-
    nth1(Row, RowMapping, TranslatedRow),
    nth1(Col, ColMapping, TranslatedCol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns all valid moves for the current game state.
valid_moves(game_state(Grid1, _, _, _, _, _), ListOfMoves) :-
    findall(move(Row, Col), validate_move(Grid1, move(Row, Col)), ListOfMoves).


% Checks if the game is over and determines the winner
game_over(game_state(Grid1, Grid2, _, _, _, _), Winner) :-
    (winning_condition(Grid1) -> Winner = player1;
     winning_condition(Grid2) -> Winner = player2;
     draw_condition(Grid1, Grid2) -> Winner = draw;
     fail).

% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Grid1, _, _, _, _, _), Player, Value) :-
    evaluate_board(Grid1, Player, Value).

% Chooses a move for the computer player based on difficulty level (level 1 should return a random valid move and level 2 the best play with a greedy algorithm)
choose_move(GameState, Level, Move) :-
    (Level = 1 -> random_move(GameState, Move);
     Level = 2 -> greedy_move(GameState, Move)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Runs the game loop, alternating turns between players
game_loop(GameState) :-
    display_game(GameState),
    (game_over(GameState, Winner) ->
        format('Game Over! Winner: ~w~n', [Winner]);
        current_player_turn(GameState, NewGameState),
        game_loop(NewGameState)).

% Handles the current player's turn
current_player_turn(game_state(Board, CurrentPlayer, Captured), NewGameState) :-
    (CurrentPlayer = human ->
        read_move(Move);
        choose_move(game_state(Board, CurrentPlayer, Captured), 2, Move)),
    move(game_state(Board, CurrentPlayer, Captured), Move, NewGameState).


% Placeholder predicates for required logic (to be implemented):
% validate_move/2, winning_condition/1, draw_condition/2, evaluate_board/3
% random_move/2, greedy_move/2, read_move/1
