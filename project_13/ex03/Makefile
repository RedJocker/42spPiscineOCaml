FILES := \
        army.ml \
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

$(BUILD_DIR)doctor.cmx : $(BUILD_DIR)people.cmx

$(BUILD_DIR)dalek.cmx : $(BUILD_DIR)people.cmx

$(BUILD_DIR)main.cmx : $(BUILD_DIR)people.cmx $(BUILD_DIR)doctor.cmx $(BUILD_DIR)army.cmx

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

