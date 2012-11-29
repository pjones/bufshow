################################################################################
NAME = bufshow
include elpkg/elpkg.mk

################################################################################
all: README.md README.html

################################################################################
README.md: $(CORE_FILE)
	echo '# Bufshow: '`$(ELPKG)/desc.sh $(CORE_FILE)`"\n" > $@
	$(ELPKG)/fdoc2md.sh bufshow-mode $(CORE_FILE) >> $@
	echo "\n# Creating a Presentation\n" >> $@
	$(ELPKG)/fdoc2md.sh bufshow-start $(CORE_FILE) >> $@

################################################################################
.PHONEY: bufshow-clean

clean: bufshow-clean

bufshow-clean:
	rm -f README.html

################################################################################
%.html: %.md
	pandoc -f markdown -t html --standalone -o $@ $<
