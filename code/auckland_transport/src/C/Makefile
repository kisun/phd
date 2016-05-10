default:
	@gcc particle_filter.c -o particle_filter.out -l sqlite3

run:
	@./particle_filter.out
	
setup:
	@ln -s ../../db/gtfs-realtime.db gtfsr.db

clean:
	@rm *.out
