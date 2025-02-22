PANDOC = pandoc
OUT = draft.pdf
SRC = tremur.rst
IMG = img

FLAGS = \
  -V "mainfont:DejaVu Serif" \
  -V "monofont:DejaVu Sans Mono" \
  --pdf-engine=xelatex \

all: $(OUT)

$(OUT): $(SRC) img/murmurtree.svg
	$(PANDOC) $(FLAGS) -f rst -t pdf -o $@ $<

img/murmurtree.svg: | $(IMG)
	dot -Tsvg dot/murmurtree.dot -o $@

$(IMG):
	mkdir -p $@

clean:
	rm -rf $(OUT) $(IMG)
