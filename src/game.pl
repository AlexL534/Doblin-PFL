% Gives access to the game menu
play :-
    main_menu.

% Displays game menu and allows game configuration
main_menu :-
    write('Welcome to Doblin!'), nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('Choose an option: '),
    read(Choice),
    configure_game(Choice, GameConfig),
    initial_state(GameConfig, GameState),
    game_loop(GameState).

configure_game(1, config(human, human, _Level1, _Level2)) :-
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

% Initializes the game state based on the configuration
initial_state(GameConfig, game_state(Board, p1, [])) :-
    initialize_board(GameConfig, Board).

% Prints game state to the terminal
display_game(game_state(Board, CurrentPlayer, _)) :-
    print_board(Board),
    format('Current Player: ~w~n', [CurrentPlayer]).

% Validates and executes a move
move(game_state(Board, CurrentPlayer, Captured), Move, game_state(NewBoard, NextPlayer, NewCaptured)) :-
    validate_move(Board, CurrentPlayer, Move),
    execute_move(Board, Move, NewBoard),
    update_captured(NewBoard, Captured, NewCaptured),
    next_player(CurrentPlayer, NextPlayer).

% Lists all valid moves for the current game state
valid_moves(game_state(Board, CurrentPlayer, _), ListOfMoves) :-
    findall(Move, validate_move(Board, CurrentPlayer, Move), ListOfMoves).

% Checks if the game is over and determines the winner
game_over(game_state(Board, CurrentPlayer, _), Winner) :-
    (winning_condition(Board, CurrentPlayer) -> Winner = CurrentPlayer;
     draw_condition(Board) -> Winner = draw;
     fail).

% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Board, _, _), Player, Value) :-
    evaluate_board(Board, Player, Value).

% Chooses a move for the computer player based on difficulty level (level 1 should return a random valid move and level 2 the best play with a greedy algorithm)
choose_move(GameState, Level, Move) :-
    (Level = 1 -> random_move(GameState, Move);
     Level = 2 -> greedy_move(GameState, Move)).

% Game loop
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


% Utility predicates (to be implemented):
% initialize_board/2, print_board/1, validate_move/3, execute_move/3
% update_captured/3, next_player/2, winning_condition/2, draw_condition/1
% evaluate_board/3, random_move/2, greedy_move/2, read_move/1
