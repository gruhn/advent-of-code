% solution(Ingredients) :-

% Each allergen is found in exactly one ingredient. 
% Each ingredient contains zero or one allergen.

contains(Ingredients, Allergen) :-
    food(Ingredients, Allergens),
    member(Allergen, Allergens).

contained(Ingredient, Allergens) :-
    food(Ingredients, Allergens),
    member(Ingredient, Ingredients).

intersection_of_ingredients(Allergen, Result) :-
    bagof(Is, contains(Is, Allergen), [I|Iss]),
    foldl(intersection, Iss, I, Result).
% only when allergen has no associated ingredient
intersection_of_ingredients(_, []).

solve(Ingredient, Allergen) :-
    intersection_of_ingredients(Allergen, [Ingredient]).
solve(Ingredient, Allergen) :-
    length(As),
    intersection_of_ingredients(A, IngredientCandidates),
    member(Ingredient, IngredientCandidates),
    \+ contains_oneof(Ingredient, As, _).

allergens(Unique) :-
    findall(Any, food(_, Any), List), 
    flatten(List, Flat), 
    sort(Flat, Unique).

% ingredient_appear_count(Ingredient, Count) :-
%    bag
	
% INPUT:

food([mxmxvkd, kfcds, sqjhc, nhms], [dairy, fish]).
food([trh, fvjkl, sbzzf, mxmxvkd], [dairy]).
food([sqjhc, fvjkl], [soy]).
food([sqjhc, mxmxvkd, sbzzf], [fish]).    

% foods --> food() | food(), "\n", foods().
% food(Is, As) --> ingredients(Is), "(contains ", allergens(As), ")".
% ingredients([I|Is]) --> ingredient(I), ingredients(Is) | [].
% allergens([A|As]) --> allergen(A), ", ", allergens(As) | [].
