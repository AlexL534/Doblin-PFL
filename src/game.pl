% -*- Prolog -*-

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Menu 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts the game by displaying the main menu
play :-
    main_menu.

% Displays the game menu and handles user choices
main_menu :-
    write('Welcome to Doblin!'), nl,
    get_grid_size(Size),
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Quit'), nl,
    write('Choose an option: '), nl,
    catch(read(Choice), error(syntax_error(_), _), fail),
    (   integer(Choice), member(Choice, [1, 2, 3, 4])
    ->  (   Choice = 4
        ->  write('Goodbye!'), nl;   
            configure_game(Choice, Size, GameConfig),
            initial_state(GameConfig, InitialState),
            game_loop(InitialState)
        );   
        write('Invalid option! Please try again.'), nl,
        main_menu
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asks for and validates grid size
get_grid_size(Size) :-
    write('Choose grid size (between 6 and 9): '),
    catch(read(Input), error(syntax_error(_), _), fail),
    (   integer(Input), Input >= 6, Input =< 9
    ->  Size = Input
    ;   write('Invalid grid size! Please choose a size between 6 and 9.'), nl,
        get_grid_size(Size)
    ).

% Ensures name does not exceed 16 characters
valid_name(Name) :-
    Name \== 'CPU',
    atom_length(Name, Length),
    Length =< 16. 

% Configures the game based on selected mode
configure_game(1, Size, config(Name1, Name2, _, _, Size)) :- 
    write('Human vs Human selected.'), nl,
    get_player_name('Player 1', Name1),
    get_player_name('Player 2', Name2).

configure_game(2, Size, config(Name1, 'CPU', Level, _, Size)) :- 
    write('Human vs Computer selected.'), nl,
    get_player_name('Your', Name1),
    get_ai_level(Level,'CPU').

configure_game(3, Size, config('CPU1', 'CPU2', Level1, Level2, Size)) :-
    write('Computer vs Computer selected.'), nl,
    get_ai_level(Level1, 'CPU1'),
    get_ai_level(Level2, 'CPU2').

% Prompts for player name
get_player_name(PlayerLabel, Name) :-
    format('Enter name for ~w: ', [PlayerLabel]),
    catch(read(Input), error(syntax_error(_), _), fail),
    (   atom(Input), valid_name(Input)
    ->  Name = Input
    ;   write('Invalid name! Please enter a name using only letters and ensure it does not exceed 16 characters.'), nl,
        get_player_name(PlayerLabel, Name)
    ).

% Prompts for AI difficulty
get_ai_level(Level, Label) :-
    ( var(Label) -> format('Choose computer difficulty (1: Easy, 2: Hard): ', []);
      format('Choose difficulty for ~w (1: Easy, 2: Hard): ', [Label])
    ),
    catch(read(Input), error(syntax_error(_), _), fail),
    (   integer(Input), member(Input, [1, 2])
    ->  Level = Input
    ;   write('Invalid difficulty! Please choose 1 or 2.'), nl,
        get_ai_level(Level, Label)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game State Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initializes the game state based on the configuration
initial_state(config(Name1, Name2, _, _, Size), game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping)) :-
    CurrentPlayer = Name1,
    initialize_grids(Size, Grid1, Grid2),
    generate_mappings(Size, RowMapping, ColMapping).

% Initializes two grids and their coordinate mappings
initialize_grids(Size, Grid1, Grid2) :-
    create_empty_grid(Size, Grid1),
    create_empty_grid(Size, Grid2).

% Creates an empty grid of the specified size, filled with '_'
create_empty_grid(Size, Grid) :-
    length(Grid, Size),
    maplist(length_(Size), Grid),
    maplist(maplist(=( '_ ')), Grid).  % Initialize each cell with '_'

% Helper predicate to define list length for rows
length_(Size, List) :-
    length(List, Size).

% Generates row and column mappings for the second grid
generate_mappings(Size, RowMapping, ColMapping) :-
    numlist(1, Size, Base),
    random_permutation(Base, RowMapping),
    random_permutation(Base, ColMapping).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display the current game state
display_game(game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping)) :-
    length(Grid1, Size),
    write('Grids:'), nl,
    Width is Size * 3 - 2,
    print_player_names(Name1, Width, 2),
    print_player_names(Name2, Width, Width + 7), nl,  
    print_column_labels(Size, ColMapping), 
    print_side_by_side(Grid1, Grid2, RowMapping),
    nl,
    print_current_player(CurrentPlayer, Name1, Name2).

print_current_player(CurrentPlayer, Name1, Name2) :-
    (   CurrentPlayer = Name1
    ->  format('Current Player: ~w~n', [Name1])  % Player 1 turn
    ;   format('Current Player: ~w~n', [Name2])  % Player 2 turn
    ).

print_player_names(Name, Width, Offset) :-
    atom(Name),  % Ensure Name is an atom
    atom_length(Name, NameLength),  % Get the length of the name
    Padding is Width - NameLength,  % Total padding needed with the dynamic offset
    LeftPadding is Padding // 2,    % Half padding for the left
    RightPadding is Padding - LeftPadding,  % Remaining padding for the right
    format('~*|~s~*|', [LeftPadding + Offset, Name, RightPadding]).

% Print column labels for both grids
print_column_labels(Size, ColMapping) :-
    write('  '), % Space before column labels for Player 1
    print_column_labels_in_order(Size),  % Write Player 1 labels
    write('   '), % Space between the grids
    print_column_labels_based_on_mapping(ColMapping),  % Write Player 2 labels
    nl.

% Print column labels for Grid 1
print_column_labels_in_order(Size) :-
    numlist(1, Size, Numbers),
    maplist(generate_column_label, Numbers, Labels),
    print_aligned_labels(Labels).

% Print column labels for Grid 2
print_column_labels_based_on_mapping(ColMapping) :-
    maplist(generate_column_label, ColMapping, MappedLabels),
    print_aligned_labels(MappedLabels).

print_aligned_labels([]).
print_aligned_labels([Label|Rest]) :-
    format('~w  ', [Label]),  % Two spaces after each label
    print_aligned_labels(Rest).

% Generate column label for a given index
generate_column_label(Index, Label) :-
    Base is 26,
    Quotient is (Index - 1) // Base,
    Remainder is (Index - 1) mod Base,
    char_code('a', A),
    (   Quotient =:= 0
    ->  Code is A + Remainder,
        char_code(Label, Code)
    ;   FirstCode is A + Quotient - 1,
        SecondCode is A + Remainder,
        char_code(First, FirstCode),
        char_code(Second, SecondCode),
        atom_codes(Label, [First, Second])
    ).

% Print both grids side by side
print_side_by_side(Grid1, Grid2, RowMapping) :-
    length(Grid1, Size),
    numlist(1, Size, Player1RowNumbers),  % Generate sequential row numbers for Player 1
    print_side_by_side_rows(Grid1, Grid2, Player1RowNumbers, RowMapping).

% Helper to print rows side by side with correct row numbers
print_side_by_side_rows([], [], [], []).
print_side_by_side_rows(Player1Rows, Player2Rows, Player1RowNumbers, Player2RowNumbers) :-
    last(Player1RowNumbers,Player1Last),
    length(Player1RowNumbers,SizeP1),
    nth1(SizeP1,Player1RowNumbers,_,RestPlayer1Rows),
    last(Player2RowNumbers,Player2Last),
    length(Player2RowNumbers,SizeP2),
    nth1(SizeP2,Player2RowNumbers,_,RestPlayer2Rows),

    last(Player1Rows,Row1),
    last(Player2Rows,Row2),
    nth1(SizeP1,Player1Rows,_,Rest1),
    nth1(SizeP2,Player2Rows,_,Rest2),
    
    format('~d ', [Player1Last]),  % Correct Player 1 row number
    print_row(Row1),  % Print Player 1 row
    write('    '),  % Space between grids
    print_row(Row2),  % Print Player 2 row
    format(' ~d', [Player2Last]),  % Correct Player 2 row number (mapped)
    nl,
    print_side_by_side_rows(Rest1, Rest2, RestPlayer1Rows, RestPlayer2Rows).

% Prints a single row
print_row([]).
print_row([Cell]) :-
    format('~w', [Cell]). 
print_row([Cell|Rest]) :-
    format('~w ', [Cell]),  
    print_row(Rest).

display_quit_message(Player) :-
    format('~w has quit the game. Thanks for playing!', [Player]), nl,
    write('==========================================='), nl,
    write('      [INFO] Player Abandonment Detected!  '), nl,
    write('==========================================='), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move Execution and Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility to find the index of an element in a list.
index_of([Element|_], Element, 1).  % The element is found at the first position.
index_of([_|Tail], Element, Index) :- 
    index_of(Tail, Element, Index1),  % Recursively look for the element
    Index is Index1 + 1.              % Increment the index as we move through the list

reverseMapping(Mapping, ReverseMapping) :-
    length(Mapping, Size),
    numlist(1, Size, InitialReverseMapping),
    reverseMappingAux(Mapping, 1, InitialReverseMapping, ReverseMapping).

reverseMappingAux([], _, ReverseMapping, ReverseMapping).
reverseMappingAux([Elem|Tail], Index, CurrentReverseMapping, ReverseMapping) :-
    nth1(Elem, CurrentReverseMapping, _, TempReverseMapping), % Remove the element at position Elem
    nth1(Elem, UpdatedReverseMapping, Index, TempReverseMapping), % Insert Index at position Elem
    NextIndex is Index + 1,
    reverseMappingAux(Tail, NextIndex, UpdatedReverseMapping, ReverseMapping).

% checks if a position is within bounds and does not already have a piece
validate_move(Grid1, move(Row, Col)) :-
    length(Grid1, Max),
    Row >= 1, Row =< Max,
    Col >= 1, Col =< Max,
    nth1(Row, Grid1, TargetRow),
    nth1(Col, TargetRow, Symbol),
    Symbol == '_ '.

% Executes a move if valid and updates the game state.
move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping), move(Row, ColLetter), game_state(NewGrid1, NewGrid2, NextPlayer, Player1, Player2, RowMapping, ColMapping)) :-
    write('Current Player: '), write(CurrentPlayer), nl,
    atom(ColLetter),
    letter_to_index(ColLetter, Col),
    (   CurrentPlayer == Player1 ->
            validate_move(Grid1, move(Row, Col)),
            NextPlayer = Player2,
            place_symbol_player1('X ', Grid1, Grid2, Row, Col, RowMapping, ColMapping, NewGrid1, NewGrid2);
        (Player2 \== 'CPU' , Player2 \== 'CPU2' -> translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol);TranslatedRow is Row,TranslatedCol is Col),
        validate_move(Grid2, move(TranslatedRow, TranslatedCol)),
        NextPlayer = Player1,
        reverseMapping(RowMapping, ReverseRowMapping),
        reverseMapping(ColMapping, ReverseColMapping),
        place_symbol_player2('O ', Grid2, Grid1, TranslatedRow, TranslatedCol, ReverseRowMapping, ReverseColMapping, NewGrid2, NewGrid1)
    ).

% Places a symbol on both grids according to the mappings.

place_symbol_player1(Symbol, Grid1, Grid2, Row, Col, RowMapping, ColMapping, NewGrid1, NewGrid2) :-
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

place_symbol_player2(Symbol, Grid1, Grid2, Row, Col, RowMappingForGrid2, ColMappingForGrid2, NewGrid1, NewGrid2) :-
    
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    translate_coordinates(Row, Col, RowMappingForGrid2, ColMappingForGrid2, TranslatedRow, TranslatedCol),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

% Updates a specific cell in a grid.
update_grid(Grid, Row, Col, Symbol, NewGrid) :-
    nth1(Row, Grid, OldRow),
    nth1(Col, OldRow, _, TempRow),
    nth1(Col, NewRow, Symbol, TempRow),
    nth1(Row, Grid, _, TempGrid),
    nth1(Row, NewGrid, NewRow, TempGrid).

% Translates coordinates for the second grid.
translate_coordinates(Row, Col, RowMapping, ColMapping, NewRow, NewCol) :-
    index_of(RowMapping, Row, NewRowIndex), 
    index_of(ColMapping, Col, NewColIndex), 
    NewRow is NewRowIndex,                  
    NewCol is NewColIndex.                  

% Map lettered columns to numeric columns
letter_to_index(a, 1).
letter_to_index(b, 2).
letter_to_index(c, 3).
letter_to_index(d, 4).
letter_to_index(e, 5).
letter_to_index(f, 6).
letter_to_index(g, 7).
letter_to_index(h, 8).
letter_to_index(i, 9).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Counts how many lines of 4 or squares a player has in their own grid
calculate_points(Grid, Player, Points) :-
    (Player = player1 -> Symbol = 'X '; Symbol = 'O '),
    findall(_, horizontal_lines(Grid, Symbol), Horizontal),
    findall(_, vertical_lines(Grid, Symbol), Vertical),
    findall(_, diagonal_lines(Grid, Symbol), Diagonals),
    findall(_, squares_of_four(Grid, Symbol), Squares),
    length(Horizontal, HorizontalCount),
    length(Vertical, VerticalCount),
    length(Diagonals, DiagonalCount),
    length(Squares, SquareCount),
    Points is HorizontalCount + VerticalCount + DiagonalCount + SquareCount,
    
    % Print the counts for each type
    format('Horizontal lines of ~w: ~w~n', [Symbol, HorizontalCount]),
    format('Vertical lines of ~w: ~w~n', [Symbol, VerticalCount]),
    format('Diagonal lines of ~w: ~w~n', [Symbol, DiagonalCount]),
    format('Squares of ~w: ~w~n', [Symbol, SquareCount]),
    
    % Print the total points
    format('Player ~w has ~w points.~n', [Player, Points]).
 

% Finds all horizontal lines of length 4 of a certain symbol
horizontal_lines(Grid, Symbol) :-
    member(Row, Grid),
    append(_, [Symbol, Symbol, Symbol, Symbol | _], Row).

% Finds all vertical lines of length 4 of a certain symbol
vertical_lines(Grid, Symbol) :-
    transpose(Grid, TransposedGrid),
    horizontal_lines(TransposedGrid, Symbol).

% Finds all diagonal lines (\ and /) of length 4 of a certain symbol
diagonal_lines(Grid, Symbol) :-
    diagonals(Grid, Diagonals),
    member(Diagonal, Diagonals),
    contains_sequence(Diagonal, [Symbol, Symbol, Symbol, Symbol]).

% Checks if a sequence of 4 symbols appears in a list (diagonal)
contains_sequence(Diagonal, Sequence) :-
    append(_, Sequence, Diagonal).

% Extracts all diagonals (\ and /) from a grid
diagonals(Grid, Diagonals) :-
    length(Grid, Size),
    findall(Diagonal, extract_diagonal(Grid, Size, Diagonal), Diagonals1),
    findall(Diagonal, extract_reverse_diagonal(Grid, Size, Diagonal), Diagonals2),
    append(Diagonals1, Diagonals2, Diagonals).

% Extracts a single diagonal from the grid (\ direction)
extract_diagonal(Grid, Size, Diagonal) :-
    MaxOffset is Size - 1,
    MinOffset is -(MaxOffset),
    between(MinOffset, MaxOffset, Offset), 
    findall(Cell, (
        between(1, Size, Row),
        Col is Row + Offset,   % For '\ ' direction
        within_bounds(Col, Size),
        nth1(Row, Grid, GridRow),
        nth1(Col, GridRow, Cell)
    ), Diagonal).

% Extracts a single diagonal from the grid ('/' direction)
extract_reverse_diagonal(Grid, Size, Diagonal) :-
    MaxOffset is Size - 1,
    MinOffset is -(MaxOffset),
    between(MinOffset, MaxOffset, Offset),
    findall(Cell, (
        between(1, Size, Row),
        Col is Row - Offset,  
        within_bounds(Col, Size),
        nth1(Row, Grid, GridRow),
        nth1(Col, GridRow, Cell)
    ), Diagonal).

% Checks if a value is within bounds
within_bounds(Value, Max) :-
    Value >= 1, Value =< Max.

% Finds all 2x2 squares of the same symbol
squares_of_four(Grid, Symbol) :-
    length(Grid, Size),
    Max is Size - 1,
    between(1, Max, Row),
    between(1, Max, Col),
    nth1(Row, Grid, Row1),
    nth1(Col, Row1, Symbol),
    NextRow is Row + 1,
    nth1(NextRow, Grid, Row2),
    nth1(Col, Row2, Symbol),
    NextCol is Col + 1,
    nth1(NextCol, Row1, Symbol),
    nth1(NextCol, Row2, Symbol).

 
% Returns all valid moves for the current game state.
valid_moves(Grid1, ListOfMoves) :-
    length(Grid1, Size),
    findall(move(Row, Letter), 
        (   between(1, Size, Row), 
            between(1, Size, Col),
            validate_move(Grid1, move(Row, Col)),
            col_to_atom(Col, Letter)
        ), 
        ListOfMoves).

col_to_atom(Col, Letter) :-
    Letters = [a, b, c, d, e, f, g, h, i],
    nth1(Col, Letters, Letter).

% Checks if the game is over and determines the winner
game_over(game_state(Grid1, Grid2, _, _, _, _, _), Winner) :-
    valid_moves(Grid1,Moves1),
    valid_moves(Grid2,Moves2),
    length(Moves1,Lmoves1),
    length(Moves2,Lmoves2),
    % No valid moves left for both players
    (   Lmoves1 == 0, Lmoves2 == 0 -> 
        calculate_points(Grid1, player1, Points1),
        calculate_points(Grid2, player2, Points2),
        (   Points1 == Points2 -> Winner = draw;
            (   Points1 < Points2 -> Winner = player1;
                Winner = player2
            )
        );
        fail
    ).

% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Grid1, _, _, _, _, _), Player, Value) :-
    evaluate_board(Grid1, Player, Value).


random_move(Grid,Move) :-
        valid_moves(Grid, ListOfMoves),
        random_member(Move, ListOfMoves).
        
% Chooses a move for the computer player based on difficulty level (level 1 should return a random valid move and level 2 the best play with a greedy algorithm)
choose_move(Grid, Level, Move) :-
    (Level = 1 -> random_move(Grid, Move),!;
     Level = 2 -> greedy_move(Grid, Move)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Runs the game loop, alternating turns between players
game_loop(GameState) :-
    display_game(GameState),
    (game_over(GameState, Winner) ->
        write('game finished'),nl,
        announce_winner(Winner),!;
        current_player_turn(GameState, NewGameState),
        (   NewGameState = quit ->
            write('Game exited by the player. Goodbye!'), nl;
            game_loop(NewGameState)
        )
    ).

announce_winner(Winner) :-
    (Winner = draw->  
        write('The game ended in a draw!'), nl;
        format('Congratulations, ~w! You Won!', [Winner]), nl
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Player Turn Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handles current player turn
current_player_turn(GameState, NewGameState) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping),
    (CurrentPlayer \== 'CPU', CurrentPlayer \== 'CPU1', CurrentPlayer \== 'CPU2' ->
        handle_player_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, NewGameState);
        handle_computer_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, NewGameState)).

% Handles a human player turn
handle_player_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, NewGameState) :-
    format('~w, it\'s your turn! Enter your move (Row,Col) or type "quit" to exit: ', [CurrentPlayer]),
    catch(read(Input), error(syntax_error(_), _), fail),
    (   Input = quit ->
        display_quit_message(CurrentPlayer),
        NewGameState = quit;
        (   Input = move(Row, Col),
            (move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping), move(Row, Col), NewGameState) -> true;
            write('Invalid move! Try again.'), nl,
            handle_player_turn(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, NewGameState))
        )
    ).

% Handles a computer player turn
handle_computer_turn(Grid1, Grid2,CurrentPlayer,Player1, Player2, RowMapping, ColMapping, NewGameState) :-
    write('Computer is thinking...'), nl,
    (CurrentPlayer == Player1 -> choose_move(Grid1, 1, Move),write('Used grid1'),nl;
     choose_move(Grid2, 1, Move),write('Used grid2'),nl),
     % Replace 1 with AI level if needed
    move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping), Move, NewGameState),
    format('Computer chose move: ~w~n', [Move]).

% Placeholder predicates for required logic (to be implemented):
% evaluate_board/3
% random_move/2, greedy_move/2
