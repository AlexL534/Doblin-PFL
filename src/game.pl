% -*- Prolog -*-

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Menu 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% play/0
% Starts the game by displaying the main menu
play :-
    main_menu.

% main_menu/0
% Displays the game menu and handles user choices
main_menu :-
    write('Welcome to Doblin!'), nl,
    get_grid_size(Size),
    display_menu,
    get_menu_choice(Choice),
    validate_choice(Choice, Size).

% display_menu/0
% Displays the game mode options
display_menu :-
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Human'), nl,
    write('4. Computer vs Computer'), nl,
    write('5. Quit'), nl,
    write('Choose an option: '), nl.

% get_menu_choice(+Choice)
% Reads and validates user menu choice
get_menu_choice(Choice) :-
    repeat,
    catch(read(Choice), error(syntax_error(_), _), fail),
    integer(Choice),
    (   Choice >= 1,
        Choice =< 5
    ),
    !.

% validate_choice(+Choice, +Size)
% Validates the menu choice
validate_choice(Choice, _) :-
    integer(Choice),
    member(Choice, [5]),
    write('Goodbye!'), nl.

validate_choice(Choice, Size) :-
    integer(Choice),
    member(Choice, [1, 2, 3, 4]),
    configure_game(Choice, Size, GameConfig),
    initial_state(GameConfig, InitialState),
    game_loop(InitialState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_grid_size(+Size)
% Asks for a grid size and validates the input
get_grid_size(Size) :-
    write('Choose grid size (between 6 and 9): '),
    repeat,
    catch(read(Input), error(syntax_error(_), _), fail),
    valid_grid_size(Input, Size).

% valid_grid_size(+Input, -Size)
% Validates grid size
valid_grid_size(Input, Size) :-
    integer(Input),
    Input >= 6,
    Input =< 9,
    Size = Input.

% valid_name(+Name)
% Ensures the name is not 'CPU', 'CPU1', or 'CPU2' and does not exceed 16 characters
valid_name(Name) :-
    Name \= 'CPU',
    Name \= 'CPU1',
    Name \= 'CPU2',
    atom_length(Name, Length),
    Length =< 16. 

% configure_game(+Mode, +Size, -GameConfig) :- 
% Configures the game based on selected mode
configure_game(1, Size, GameConfig) :- 
    GameConfig = config(Name1, Name2, _, _, Size),
    write('Human vs Human selected.'), nl,
    get_player_name('Player 1', Name1),
    get_player_name('Player 2', Name2).

configure_game(2, Size, GameConfig ) :- 
    GameConfig = config(Name1, 'CPU', _, Level, Size),
    write('Human vs Computer selected.'), nl,
    get_player_name('Player 1', Name1),
    get_ai_level('CPU', Level).

configure_game(3, Size, GameConfig ) :- 
    GameConfig = config('CPU', Name2, Level, _, Size),
    write('Computer vs Human selected.'), nl,
    get_ai_level('CPU', Level),
    get_player_name('Player 2', Name2).

configure_game(4, Size, GameConfig ) :-
    GameConfig = config('CPU1', 'CPU2', Level1, Level2, Size),
    write('Computer vs Computer selected.'), nl,
    get_ai_level('CPU1', Level1),
    get_ai_level('CPU2', Level2).

% get_player_name(+PlayerLabel, -Name)
% Asks for player name and validates it
get_player_name(PlayerLabel, Name) :-
    repeat,
    format('Enter name for ~w (max 16 chars and only lowercase letters): ', [PlayerLabel]),
    catch(read(Name), error(syntax_error(_), _), fail),
    valid_name(Name),
    !.

% get_ai_level(+CPUName, -Level)
% Asks for AI difficulty level
get_ai_level(CPUName, Level) :-
    format('Choose difficulty for ~w (1: Easy, 2: Hard): ', [CPUName]),
    catch(read(Input), error(syntax_error(_), _), fail),
    validate_difficulty(CPUName, Input, Level).

% validate_difficulty(+CPUName, +Input, -Level)
% Validates AI difficulty level input
validate_difficulty(_, 1, 1).
validate_difficulty(_, 2, 2).
validate_difficulty(CPUName, _, Level) :-
    write('Invalid difficulty! Please choose 1 or 2.'), nl,
    get_ai_level(CPUName, Level).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game State Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initial_state(+GameConfig, -GameState)
% Initializes the game state based on the provided configuration
initial_state(GameConfig, GameState) :-
    GameConfig = config(Name1, Name2, AI1Level, AI2Level, Size),
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping, AI1Level, AI2Level),
    CurrentPlayer = Name1,
    initialize_grids(Size, Grid1, Grid2),
    generate_mappings(Size, RowMapping, ColMapping).

% initialize_grids(+Size, -Grid1, -Grid2)
% Initializes two empty grids of the specified size
initialize_grids(Size, Grid1, Grid2) :-
    create_empty_grid(Size, Grid1),
    create_empty_grid(Size, Grid2).

% create_empty_grid(+Size, -Grid)
% Creates an empty grid of the specified size, filled with '_ '
create_empty_grid(Size, Grid) :-
    length(Grid, Size),
    maplist(length_(Size), Grid),
    maplist(maplist(=( '_ ')), Grid).

% length_(+Size, -List)
% Helper predicate to define the length of a list (for row creation)
length_(Size, List) :-
    length(List, Size).

% generate_mappings(+Size, -RowMapping, -ColMapping)
% Generates random row and column mappings for the second grid
generate_mappings(Size, RowMapping, ColMapping) :-
    numlist(1, Size, Base),
    random_permutation(Base, RowMapping),
    random_permutation(Base, ColMapping).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display_game(+GameState)
% Displays the current game state

display_game(GameState) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping, _, _),
    length(Grid1, Size),
    nl,
    Width is Size * 3 - 2,
    print_player_names(Name1, Width, 2),
    print_player_names(Name2, Width, Width + 7), nl,  
    print_column_labels(Size, ColMapping), 
    print_side_by_side(Grid1, Grid2, RowMapping),
    nl,
    print_current_player(CurrentPlayer).

% print_current_player(+CurrentPlayer)
% Prints the current player
print_current_player(CurrentPlayer) :-
    format('Current Player: ~w~n', [CurrentPlayer]).

% print_player_names(+Name, +Width, +Offset)
% Prints player names with dynamic padding based on the width and offset
print_player_names(Name, Width, Offset) :-
    atom(Name),  % Ensure Name is an atom
    atom_length(Name, NameLength), 
    Padding is Width - NameLength, 
    LeftPadding is Padding // 2,    
    RightPadding is Padding - LeftPadding, 
    format('~*|~s~*|', [LeftPadding + Offset, Name, RightPadding]).

% print_column_labels(+Size, +ColMapping)
% Print column labels for both grids
print_column_labels(Size, ColMapping) :-
    write('  '), % Space before column labels for Player 1
    print_column_labels_in_order(Size),  % Write Player 1 labels
    write('   '), % Space between the grids
    print_column_labels_based_on_mapping(ColMapping),  % Write Player 2 labels
    nl.

% print_column_labels_in_order(+Size)
% Print column labels for Grid 1
print_column_labels_in_order(Size) :-
    numlist(1, Size, Numbers),
    maplist(generate_column_label, Numbers, Labels),
    print_aligned_labels(Labels).

% print_column_labels_based_on_mapping(+ColMapping)
% Print column labels for Grid 2
print_column_labels_based_on_mapping(ColMapping) :-
    maplist(generate_column_label, ColMapping, MappedLabels),
    print_aligned_labels(MappedLabels).

% print_aligned_labels(+Labels)
% Prints the list of column labels with correct alignment
print_aligned_labels([]).
print_aligned_labels([Label|Rest]) :-
    format('~w  ', [Label]),  % Two spaces after each label
    print_aligned_labels(Rest).

% generate_column_label(+Index, -Label)
% Generates a column label for a given index
generate_column_label(Index, Label) :-
    Base is 26,
    Quotient is (Index - 1) // Base,
    Remainder is (Index - 1) mod Base,
    char_code('a', A),
    Quotient =:= 0,
    Code is A + Remainder,
    char_code(Label, Code), 
    !.

generate_column_label(Index, Label) :-
    Base is 26,
    Quotient is (Index - 1) // Base,
    Remainder is (Index - 1) mod Base,
    char_code('a', A),
    FirstCode is A + Quotient - 1,
    SecondCode is A + Remainder,
    char_code(First, FirstCode),
    char_code(Second, SecondCode),
    atom_codes(Label, [First, Second]).

% print_side_by_side(+Grid1, +Grid2, +RowMapping)
% Print both grids side by side
print_side_by_side(Grid1, Grid2, RowMapping) :-
    length(Grid1, Size),
    numlist(1, Size, Player1RowNumbers),  % Generate sequential row numbers for Player 1
    print_side_by_side_rows(Grid1, Grid2, Player1RowNumbers, RowMapping).

% print_side_by_side_rows(+Player1Rows, +Player2Rows, +Player1RowNumbers, +Player2RowNumbers)
% Helper to print rows side by side with correct row numbers
print_side_by_side_rows([], [], [], []).
print_side_by_side_rows(Player1Rows, Player2Rows, Player1RowNumbers, Player2RowNumbers) :-
    last(Player1RowNumbers, Player1Last),
    length(Player1RowNumbers, SizeP1),
    nth1(SizeP1, Player1RowNumbers, _, RestPlayer1Rows),
    last(Player2RowNumbers, Player2Last),
    length(Player2RowNumbers, SizeP2),
    nth1(SizeP2, Player2RowNumbers, _, RestPlayer2Rows),

    last(Player1Rows, Row1),
    last(Player2Rows, Row2),
    nth1(SizeP1, Player1Rows, _, Rest1),
    nth1(SizeP2, Player2Rows, _, Rest2),
    
    format('~d ', [Player1Last]),  % Correct Player 1 row number
    print_row(Row1),  % Print Player 1 row
    write('    '),  % Space between grids
    print_row(Row2),  % Print Player 2 row
    format(' ~d', [Player2Last]),  % Correct Player 2 row number (mapped)
    nl,
    print_side_by_side_rows(Rest1, Rest2, RestPlayer1Rows, RestPlayer2Rows).

% print_row(+Row)
% Prints a single row
print_row([]).
print_row([Cell]) :-
    format('~w', [Cell]). 
print_row([Cell|Rest]) :-
    format('~w ', [Cell]),  
    print_row(Rest).

% display_quit_message(+Player)
% Displays a message when a player quits the game
display_quit_message(Player) :-
    write('-------------------------------------------'), nl,
    write('      [INFO] Player Abandonment Detected!  '), nl,
    write('-------------------------------------------'), nl,
    format('~w has quit the game. Thanks for playing!', [Player]), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move Execution and Validation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% index_of(+List, +Element, -Index)
% Finds the index of an element in a list. Indexing starts from 1
index_of([Element|_], Element, 1). 
index_of([_|Tail], Element, Index) :- 
    index_of(Tail, Element, Index1),  
    Index is Index1 + 1.              

% reverseMapping(+Mapping, -ReverseMapping)
% Generates a reverse mapping for translating coordinates from grid1 to grid2
reverseMapping(Mapping, ReverseMapping) :-
    length(Mapping, Size),
    numlist(1, Size, InitialReverseMapping),
    reverseMappingAux(Mapping, 1, InitialReverseMapping, ReverseMapping).


% reverseMappingAux(+Mapping, +Index, +CurrentReverseMapping, -ReverseMapping)
% Recursive helper predicate to build the reverse mapping
reverseMappingAux([], _, ReverseMapping, ReverseMapping).
reverseMappingAux([Elem|Tail], Index, CurrentReverseMapping, ReverseMapping) :-
    nth1(Elem, CurrentReverseMapping, _, TempReverseMapping), % Remove the element at position Elem
    nth1(Elem, UpdatedReverseMapping, Index, TempReverseMapping), % Insert Index at position Elem
    NextIndex is Index + 1,
    reverseMappingAux(Tail, NextIndex, UpdatedReverseMapping, ReverseMapping).

% validate_move(+Grid1, +move(Row, Col))
% Checks if a move is within bounds and      target cell is empty ('_ ')
validate_move(Grid1, move(Row, Col)) :-
    length(Grid1, Max),
    Row >= 1, Row =< Max,
    Col >= 1, Col =< Max,
    nth1(Row, Grid1, TargetRow),
    nth1(Col, TargetRow, Symbol),
    Symbol = '_ '.

% move(+State, +Move, -NewGameState)
% Executes a move if valid and updates the game state.
move(game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level), move(Row, ColLetter), game_state(NewGrid1, NewGrid2, NextPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level)) :-
    atom(ColLetter),
    letter_to_index(ColLetter, Col),
    handle_player_move(CurrentPlayer, Player1, Row, Col, Grid1, Grid2, RowMapping, ColMapping, NewGrid1, NewGrid2, NextPlayer, Player2, _, _).

% handle_player_move(+CurrentPlayer, +Player1, +Row, +Col, +Grid1, +Grid2, +RowMapping, +ColMapping, -NewGrid1, -NewGrid2, -NextPlayer, +Player2, +TranslatedRow, +TranslatedCol)
% Handles Player 1 move
handle_player_move(CurrentPlayer, Player1, Row, Col, Grid1, Grid2, RowMapping, ColMapping, NewGrid1, NewGrid2, NextPlayer, Player2, _, _) :-
    CurrentPlayer = Player1,
    validate_move(Grid1, move(Row, Col)),
    NextPlayer = Player2,
    place_symbol('X ', Grid1, Grid2, Row, Col, RowMapping, ColMapping, NewGrid1, NewGrid2).

% Handles Player 2 move
handle_player_move(CurrentPlayer, Player1, Row, Col, Grid1, Grid2, RowMapping, ColMapping, NewGrid1, NewGrid2, NextPlayer, Player2, TranslatedRow, TranslatedCol) :-
    CurrentPlayer \= Player1,
    handle_player2_coordinates(Player2, Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    validate_move(Grid2, move(TranslatedRow, TranslatedCol)),
    NextPlayer = Player1,
    reverseMapping(RowMapping, ReverseRowMapping),
    reverseMapping(ColMapping, ReverseColMapping),
    place_symbol('O ', Grid2, Grid1, TranslatedRow, TranslatedCol, ReverseRowMapping, ReverseColMapping, NewGrid2, NewGrid1).

% handle_player2_coordinates(+Player2, +Row, +Col, +RowMapping, +ColMapping, -TranslatedRow, -TranslatedCol)
% Determines the coordinates for Player 2 based on whether they are a CPU or human
handle_player2_coordinates(Player2, Row, Col, _RowMapping, _ColMapping, TranslatedRow, TranslatedCol) :-
    Player2 = 'CPU',
    TranslatedRow = Row,
    TranslatedCol = Col.

handle_player2_coordinates(Player2, Row, Col, _RowMapping, _ColMapping, TranslatedRow, TranslatedCol) :-
    Player2 = 'CPU2',
    TranslatedRow = Row,
    TranslatedCol = Col.

handle_player2_coordinates(_, Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol) :-
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol).

% place_symbol(+Symbol, +Grid1, +Grid2, +Row, +Col, +RowMapping, +ColMapping, -NewGrid1, -NewGrid2)
% Places a symbol on both grids according to the mappings.
place_symbol(Symbol, Grid1, Grid2, Row, Col, RowMapping, ColMapping, NewGrid1, NewGrid2) :-
    update_grid(Grid1, Row, Col, Symbol, NewGrid1),
    translate_coordinates(Row, Col, RowMapping, ColMapping, TranslatedRow, TranslatedCol),
    update_grid(Grid2, TranslatedRow, TranslatedCol, Symbol, NewGrid2).

% update_grid(+Grid, +Row, +Col, +Symbol, -NewGrid)
% Updates a specific cell in a grid.
update_grid(Grid, Row, Col, Symbol, NewGrid) :-
    nth1(Row, Grid, OldRow),
    nth1(Col, OldRow, _, TempRow),
    nth1(Col, NewRow, Symbol, TempRow),
    nth1(Row, Grid, _, TempGrid),
    nth1(Row, NewGrid, NewRow, TempGrid).

% translate_coordinates(+Row, +Col, +RowMapping, +ColMapping, -NewRow, -NewCol)
% Translates coordinates depending on the provided mapping
translate_coordinates(Row, Col, RowMapping, ColMapping, NewRow, NewCol) :-
    index_of(RowMapping, Row, NewRowIndex), 
    index_of(ColMapping, Col, NewColIndex), 
    NewRow is NewRowIndex,                  
    NewCol is NewColIndex.                  

% letter_to_index(+Letter, -Index)
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

% calculate_points(+Grid, +Player, +Player1, -Points)
% Calculates total points of a player based on how many lines of 4 or squares a player has in their own grid
calculate_points(Grid, Player, Player1, Points) :-
    get_symbol(Player, Player1, Symbol),
    findall(_, horizontal_lines(Grid, Symbol), Horizontal),
    findall(_, vertical_lines(Grid, Symbol), Vertical),
    findall(_, diagonal_lines(Grid, Symbol), Diagonals),
    findall(_, squares_of_four(Grid, Symbol), Squares),
    length(Horizontal, HorizontalCount),
    length(Vertical, VerticalCount),
    length(Diagonals, DiagonalCount),
    length(Squares, SquareCount),
    Points is HorizontalCount + VerticalCount + DiagonalCount + SquareCount.

% get_symbol(+Player, +Player1, -Symbol)
% Assigns the correct symbol based on the player
get_symbol(Player, Player1, 'X ') :- Player = Player1.
get_symbol(Player, Player1, 'O ') :- Player \= Player1.

% horizontal_lines(+Grid, +Symbol)
% Finds all horizontal lines of length 4 of a certain symbol
horizontal_lines(Grid, Symbol) :-
    member(Row, Grid),
    append(_, [Symbol, Symbol, Symbol, Symbol | _], Row).

% vertical_lines(+Grid, +Symbol)
% Finds all vertical lines of length 4 of a certain symbol
vertical_lines(Grid, Symbol) :-
    transpose(Grid, TransposedGrid),
    horizontal_lines(TransposedGrid, Symbol).

% diagonal_lines(+Grid, +Symbol)
% Finds all diagonal lines (\ and /) of length 4 of a certain symbol
diagonal_lines(Grid, Symbol) :-
    diagonals(Grid, Diagonals),
    member(Diagonal, Diagonals),
    contains_sequence(Diagonal, [Symbol, Symbol, Symbol, Symbol]).

% contains_sequence(+Diagonal, +Sequence)
% Checks if a sequence exists in the diagonal
contains_sequence(Diagonal, Sequence) :-
    append(_, Sequence, Diagonal).

% diagonals(+Grid, -Diagonals)
% Extracts all diagonals (\ and /) from the grid
diagonals(Grid, Diagonals) :-
    length(Grid, Size),
    findall(Diagonal, extract_diagonal(Grid, Size, Diagonal), Diagonals1),
    findall(Diagonal, extract_reverse_diagonal(Grid, Size, Diagonal), Diagonals2),
    append(Diagonals1, Diagonals2, Diagonals).

% extract_diagonal(+Grid, +Size, -Diagonal)
% Extracts a single \ diagonal of length 4 from the grid
extract_diagonal(Grid, Size, Diagonal) :-
    between(1, Size, Row),    
    between(1, Size, Col),  
    findall(Cell, (
        between(0, 3, Offset),
        RowOffset is Row + Offset,
        ColOffset is Col + Offset,
        within_bounds(RowOffset, Size),
        within_bounds(ColOffset, Size),
        nth1(RowOffset, Grid, GridRow),
        nth1(ColOffset, GridRow, Cell)
    ), Diagonal),
    length(Diagonal, 4).

% extract_reverse_diagonal(+Grid, +Size, -Diagonal)
% Extracts a single / diagonal of length 4 from the grid
extract_reverse_diagonal(Grid, Size, Diagonal) :-
    between(1, Size, Row),  
    between(1, Size, Col), 
    findall(Cell, (
        between(0, 3, Offset), 
        RowOffset is Row + Offset,
        ColOffset is Col - Offset,
        within_bounds(RowOffset, Size),
        within_bounds(ColOffset, Size),
        nth1(RowOffset, Grid, GridRow),
        nth1(ColOffset, GridRow, Cell)
    ), Diagonal),
    length(Diagonal, 4).

% within_bounds(+Value, +Max)
% Checks if a value is within grid bounds
within_bounds(Value, Max) :-
    Value >= 1, Value =< Max.

% squares_of_four(+Grid, +Symbol)
% Finds all 2x2 squares of the same symbol in the grid
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

% valid_moves(+Grid1, -ListOfMoves)
% Returns all valid moves for the current game state
valid_moves(Grid1, ListOfMoves) :-
    length(Grid1, Size),
    findall(move(Row, Letter), 
        (   between(1, Size, Row), 
            between(1, Size, Col),
            validate_move(Grid1, move(Row, Col)),
            col_to_atom(Col, Letter)
        ), 
        ListOfMoves).

% col_to_atom(+Col, -Letter)
% Translates a column number to its respective letter 
col_to_atom(Col, Letter) :-
    Letters = [a, b, c, d, e, f, g, h, i],
    nth1(Col, Letters, Letter).

% game_over(+game_state, -Winner)
% Checks if the game is over and determines the winner
game_over(game_state(Grid1, Grid2, _, _, _, _, _, _, _), Winner) :-
    valid_moves(Grid2, Moves2),
    length(Moves2, Lmoves2),
    % No valid moves left for player 2 (he always does last move)
    Lmoves2 = 0,
    calculate_points(Grid1, player1, player1, Points1),
    calculate_points(Grid2, player2, player1, Points2),
    format('Player 1 (X) Points: ~w~n', [Points1]),
    format('Player 2 (O) Points: ~w~n', [Points2]),
    check_winner(Points1, Points2, Winner).

% check_winner(+Points1, +Points2, -Winner)
% Determines the winner based on points
check_winner(Points1, Points2, draw) :-
    Points1 = Points2.

check_winner(Points1, Points2, player1) :-
    Points1 < Points2.

check_winner(Points1, Points2, player2) :-
    Points1 > Points2.

% value(+game_state, +Player, -Value)
% Evaluates the current game state and returns how bad or good it is for the current player
value(game_state(Grid1, _, _, _, _, _), Player, Value) :-
    evaluate_board(Grid1, Player, Value).

% random_move(+Grid, -Move)
% Chooses a random valid move
random_move(Grid, Move) :-
    valid_moves(Grid, ListOfMoves),
    random_member(Move, ListOfMoves).

% get_best_move(+Grid, +Points, +Difference, +Player, +Player1, +ListOfMoves, +CurrentBest, -BestMove)
% Finds the best move by simulating all valid moves and selecting the one that minimizes the difference in points
get_best_move(_, _, _, _, _, [], Move, Move) :-!.
get_best_move(Grid, Points, Difference, Player, Player1, [Move|ListOfMoves], CurrentBest, BestMove) :-
    Move = move(Row, ColLetter),
    atom(ColLetter),
    letter_to_index(ColLetter, Col),
    get_symbol(Player, Player1, Symbol),
    update_grid(Grid, Row, Col, Symbol, NewGrid), % simulates the move
    calculate_points(NewGrid, Player, Player1, UpdatedPoints), % calculates the amount of points the player gets when that move is played
    NewDifference is UpdatedPoints-Points, % calculates amout of points gained
    compare_best_move(NewDifference, Difference, Grid, Points, Player, Player1, ListOfMoves, Move, BestMove, CurrentBest).
    
% compare_best_move(+NewDifference, +Difference, +Grid, +Points, +Player, +Player1, +ListOfMoves, +Move, +BestMove, +CurrentBest)
% Compares the current move points with the best move points and updates the best move if necessary
compare_best_move(NewDifference, Difference, Grid, Points, Player, Player1, ListOfMoves, Move, BestMove, _) :-
    NewDifference =< Difference,
    get_best_move(Grid, Points, NewDifference, Player, Player1, ListOfMoves, Move, BestMove).

compare_best_move(NewDifference, Difference, Grid, Points, Player, Player1, ListOfMoves, _, BestMove, CurrentBest) :-
    NewDifference > Difference,
    get_best_move(Grid, Points, Difference, Player, Player1, ListOfMoves, CurrentBest, BestMove).

% greedy_move(+Grid, +Player, +Player1, -Move)
% Uses the get_best_move predicate to find the move that minimizes points in each turn
greedy_move(Grid, Player, Player1, Move) :-
    valid_moves(Grid, ListOfMoves),
    calculate_points(Grid, Player, Player1, Points),
    get_best_move(Grid, Points, 999, Player, Player1, ListOfMoves, move(-1,-1), Move).

% choose_move(+GameState, +Level, -Move)
% Chooses the next move based on the selected mode (random or greedy)
choose_move(GameState, Level, Move) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level),
    Level = 1,
    CurrentPlayer=Player1,
    random_move(Grid1, Move).

choose_move(GameState, Level, Move) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level),
    Level = 1,
    CurrentPlayer=Player2,
    random_move(Grid2, Move).

choose_move(GameState, Level, Move) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level),
    Level = 2,
    CurrentPlayer=Player1,
    greedy_move(Grid1, CurrentPlayer, Player1, Move).

choose_move(GameState, Level, Move) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Player1, Player2, RowMapping, ColMapping, AI1Level, AI2Level),
    Level = 2,
    CurrentPlayer=Player2,
    greedy_move(Grid2, CurrentPlayer, Player1, Move).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% game_loop(+GameState)
% Runs the game loop, alternating turns between players
game_loop(GameState) :-
    display_game(GameState),
    check_game_status(GameState).

% check_game_status(+GameState)
% Checks if the game is over or continues with the next player turn
check_game_status(GameState) :-
    game_over(GameState, Winner),
    !,
    announce_winner(Winner).

check_game_status(GameState) :-
    current_player_turn(GameState, NewGameState),
    handle_next_state(NewGameState).

% handle_next_state(+NewGameState)
% Handle the next game state (either quit or continue the loop)
handle_next_state(quit) :- !.
handle_next_state(NewGameState) :- game_loop(NewGameState).

% announce_winner(+Winner)
% Announces the winner of the game
announce_winner(Winner) :-
    Winner = draw,
    write('The game ended in a draw!'), nl.

announce_winner(Winner) :-
        format('Congratulations, ~w! You Won!', [Winner]), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Player Turn Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% current_player_turn(+GameState, -NewGameState)
% Handles current player turn
current_player_turn(GameState, NewGameState) :-
    GameState = game_state(_, _, CurrentPlayer, _, _, _, _, _, _),
    handle_turn(GameState, CurrentPlayer, NewGameState).

% handle_turn(+GameState, +CurrentPlayer, -NewGameState)
% Handles player turn based on if he is human or computer
handle_turn(GameState, CurrentPlayer, NewGameState) :-
    CurrentPlayer \= 'CPU',
    CurrentPlayer \= 'CPU1',
    CurrentPlayer \= 'CPU2',
    handle_player_turn(GameState, CurrentPlayer, NewGameState).

% Handle computer turn otherwise for 'CPU'
handle_turn(GameState, CurrentPlayer, NewGameState) :-
    CurrentPlayer = 'CPU',
    handle_computer_turn(GameState, NewGameState).

% Handle computer turn otherwise for 'CPU1'
handle_turn(GameState, CurrentPlayer, NewGameState) :-
    CurrentPlayer = 'CPU1',
    handle_computer_turn(GameState, NewGameState).

% Handle computer turn otherwise for 'CPU2'
handle_turn(GameState, CurrentPlayer, NewGameState) :-
    CurrentPlayer = 'CPU2',
    handle_computer_turn(GameState, NewGameState).

% handle_player_turn(+GameState, +CurrentPlayer, -NewGameState)
% Handles a human player turn
handle_player_turn(GameState, CurrentPlayer, NewGameState) :-
    format('~w, it\'s your turn! Enter your move (Row,Col) or type "quit" to exit: ', [CurrentPlayer]),
    read(Input),
    check_input(Input, GameState, CurrentPlayer, NewGameState).

% check_input(+Input, +GameState, +CurrentPlayer, -NewGameState)
% Validates player input (either a move or quit) and performs the corresponding action
check_input(quit, GameState, CurrentPlayer, quit) :-
    display_quit_message(CurrentPlayer).

check_input(move(Row, Col), GameState, CurrentPlayer, NewGameState) :-
    move(GameState, move(Row, Col), NewGameState),
    !.

check_input(_, GameState, CurrentPlayer, NewGameState) :-
    write('Invalid move! Try again.'), nl,
    handle_player_turn(GameState, CurrentPlayer, NewGameState).
    
% handle_computer_turn(+GameState, -NewGameState)
% Handles a computer player turn
handle_computer_turn(GameState, NewGameState) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping, AI1Level, AI2Level),
    CurrentPlayer = Name1,
    choose_move(GameState, AI1Level, Move),
    % Replace 1 with AI level if needed
    move(GameState, Move, NewGameState),
    format('Computer chose move: ~w~n', [Move]).

handle_computer_turn(GameState, NewGameState) :-
    GameState = game_state(Grid1, Grid2, CurrentPlayer, Name1, Name2, RowMapping, ColMapping, AI1Level, AI2Level),
    CurrentPlayer = Name2,
    choose_move(GameState, AI2Level, Move),
    move(GameState, Move, NewGameState),
    % operations required to display the move with the grid2 coordinates
    Move = move(Row, ColLetter),
    letter_to_index(ColLetter, Col),
    reverseMapping(RowMapping, RRowMapping),
    reverseMapping(ColMapping, RColMapping),
    translate_coordinates(Row, Col, RRowMapping, RColMapping, TranslatedRow, TranslatedCol),
    col_to_atom(TranslatedCol, TranslatedColLetter),
    
    format('Computer chose move: move(~d,~w)~n', [TranslatedRow, TranslatedColLetter]).
