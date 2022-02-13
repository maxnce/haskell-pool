##
## EPITECH PROJECT, 2021
## 110borwein
## File description:
## Makefile
##

CFLAGS = -Wall -Wextra -Wpedantic -g3 -I./include -lm

NCURSES = -lncurses

CSFML = -l csfml-graphics -l csfml-window -l csfml-system -l csfml-network -l csfml-audio

SRC	=	DoOp.hs \

NAME	=	doop

all:	$(NAME)

$(NAME):
		@ghc $(SRC) -o $(NAME)

clean:
	rm -f *.hi *.o

fclean: clean
	rm -rf $(NAME)

re: fclean all

.PHONY = all re clean fclean