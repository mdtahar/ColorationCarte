CAML=ocamlc
EXC=main 
LIBS=graphics.cma

all:$(EXC)

$(EXC) : example.cmo sat_solver.cmo main.cmo jeu.cmo affiche.cmo solution.cmo
	$(CAML) $(LIBS) -o $(EXC) $^

main.cmo :main.ml affiche.cmo jeu.cmo solution.cmo  example.cmo  
	$(CAML) -c $^ 
sat_solver.cmo : sat_solver.ml sat_solver.cmi
	$(CAML) -c  sat_solver.ml

sat_solver.cmi: sat_solver.mli
	$(CAML) -c  sat_solver.mli

example.cmo : example.ml jeu.cmo  
	$(CAML) -c $^

solution.cmo : solution.ml jeu.cmo 
	$(CAML) -c $^ 

jeu.cmo : jeu.ml sat_solver.cmo
	$(CAML) -c jeu.ml sat_solver.cmo

affiche.cmo :affiche.ml jeu.cmo solution.cmo 		
	$(CAML) -c affiche.ml jeu.cmo solution.cmo
clean : 
	rm -rf  *~
mrproper : 
	rm -rf *.cmo *.cmi 
