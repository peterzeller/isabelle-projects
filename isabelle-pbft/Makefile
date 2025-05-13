watch:
	while inotifywait -e close_write *.thy; do make build; done

build:
	isabelle build -D .
