from pyswip import Prolog
p = Prolog()
p.consult('logic.pl')
query = "prolog_a_star([car(b,3,0,h,3), car(c,2,4,h,2), car(d,0,5,h,3), car(e,3,5,h,2), car(f,0,0,v,3), car(g,1,3,v,2), car(h,2,0,v,2), car(i,3,1,v,3), car(j,4,3,v,2), car(k,5,2,v,2), car(l,5,4,v,2), car(r,1,2,h,2)], Path, Cost, Nodes)"
sol = next(p.query(query))
path = sol['Path']
print('Path type:', type(path))
print('Path repr:', path)
if isinstance(path, list):
    for i, move in enumerate(path):
        print(i, type(move), move)
