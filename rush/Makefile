##
## EPITECH PROJECT, 2022
## pushswap_checker
## File description:
## Makefile
##

SRC	=	src/Main.hs	\
		src/PushswapFunctions.hs \

NAME	=	pushswap_checker

all:	$(NAME)

$(NAME):
		@ghc $(SRC) -o $(NAME)

clean:
	rm -f src/*.hi src/*.o

fclean: clean
	rm -rf $(NAME)

re: fclean all

.PHONY = all re clean fclean