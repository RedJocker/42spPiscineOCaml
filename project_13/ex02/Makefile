FILES := \
	people.ml \
        dalek.ml \
	doctor.ml \
	main.ml

BUILD_DIR := ./build/

DOT_CMX  := $(addprefix $(BUILD_DIR), $(patsubst %.ml, %.cmx, $(FILES)))

APP_NAME := people
OCAMLC := ocamlopt

all: $(APP_NAME)

$(APP_NAME): $(DOT_CMX)
	$(OCAMLC) -o $@ $^


$(DOT_CMX): $(BUILD_DIR)%.cmx : %.ml | $(BUILD_DIR)
	$(OCAMLC) -I $(BUILD_DIR) -c -o $@ $^

./build/doctor.cmx : ./build/people.cmx

./build/dalek.cmx : ./build/people.cmx

./build/main.cmx : ./build/people.cmx ./build/doctor.cmx

$(BUILD_DIR):
	@-mkdir -p $@

.PHONY: all run clean fclean re 

run: all
	./$(APP_NAME)

clean:
	@-rm -rf $(BUILD_DIR)

fclean: clean
	@-rm ./$(APP_NAME)

re: fclean all

