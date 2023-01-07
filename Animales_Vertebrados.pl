%Hechos
animal(ovejas).
animal(cabras).
animal(alces).
animal(ciervos).
animal(camellos).
animal(lobos).
animal(leones).
animal(mapaches).
animal(zorros).
animal(perros).
animal(murcielagos_blanco).
animal(murcielagos_pata_peluda).
animal(murcielagos_lengueton).
animal(murcielagos_trompudos).
animal(ballenas).
animal(delfines).
animal(orcas).
animal(cachalotes).
animal(ratas).
animal(ardillas).
animal(castores).
animal(capibaras).
animal(canguros).
animal(koala).
animal(numbat).
animal(avestruz).
animal(kiwis).
animal(gallinas).
animal(patos).
animal(gaviotas).
animal(pez_globo).
animal(pez_loro).
animal(pez_payaso).
animal(pez_martillo).
animal(sardina).
animal(camaleon).
animal(gorgona).
animal(anolis).
animal(caiman).
animal(cocodrilo).
animal(cobra).
animal(mamba_negra).
animal(cascabel).
animal(piton).
animal(tortuga_laud).
animal(tortuga_verde).
animal(tortuga_carey).
animal(tortuga_bastarda).
animal(ranas).
animal(sapos).
animal(triton).
animal(salamandras).
animal(gallipato).
animal(vertebrados).
animal(mamiferos).
animal(aves).
animal(peces).
animal(reptiles).
animal(anfibios).
animal(herbivoros).
animal(carnivoros).
animal(murcielagos).
animal(cetaceos).
animal(roedores).
animal(marsupiales).
animal(lagartos).
animal(serpientes).
animal(tortugas).

animales(vertebrados,mamiferos).
animales(vertebrados,aves).
animales(vertebrados,peces).
animales(vertebrados,reptiles).
animales(vertebrados,anfibios).
animales(mamiferos,herbivoros).
animales(mamiferos,carnivoros).
animales(mamiferos,murcielagos).
animales(mamiferos,cetaceos).
animales(mamiferos,roedores).
animales(mamiferos,marsupiales).
animales(herbivoros,ovejas).
animales(herbivoros,cabras).
animales(herbivoros,alces).
animales(herbivoros,ciervos).
animales(herbivoros,camellos).
animales(carnivoros,lobos).
animales(carnivoros,leones).
animales(carnivoros,mapaches).
animales(carnivoros,zorros).
animales(carnivoros,perros).
animales(murcielagos,murcielagos_blanco).
animales(murcielagos,murcielagos_pata_peluda).
animales(murcielagos,murcielagos_lengueton).
animales(murcielagos,murcielagos_trompudos).
animales(cetaceos,ballenas).
animales(cetaceos,delfines).
animales(cetaceos,orcas).
animales(cetaceos,cachalotes).
animales(roedores,ratas).
animales(roedores,ardillas).
animales(roedores,castores).
animales(roedores,capibaras).
animales(marsupiales,canguros).
animales(marsupiales,koala).
animales(marsupiales,numbat).
animales(aves,avestruz).
animales(aves,kiwis).
animales(aves,gallinas).
animales(aves,patos).
animales(aves,gaviotas).
animales(peces,pez_globo).
animales(peces,pez_loro).
animales(peces,pez_payaso).
animales(peces,pez_martillo).
animales(peces,sardina).
animales(reptiles,lagartos).
animales(reptiles,serpientes).
animales(reptiles,tortugas).
animales(lagartos,camaleon).
animales(lagartos,gorgona).
animales(lagartos,anolis).
animales(lagartos,caiman).
animales(lagartos,cocodrilo).
animales(serpientes,cobra).
animales(serpientes,mamba_negra).
animales(serpientes,cascabel).
animales(serpientes,piton).
animales(tortugas,tortuga_laud).
animales(tortugas,tortuga_verde).
animales(tortugas,tortuga_carey).
animales(tortugas,tortuga_bastarda).
animales(anfibios,ranas).
animales(anfibios,sapos).
animales(anfibios,triton).
animales(anfibios,salamandras).
animales(anfibios,gallipato).


%REGLAS

especies(X,Y):- animal(X),animales(X,Y).
misma_familias(X,Y):- animales(Z,X),animales(Z,Y), not(X==Y).
parientes(X,Y):- animal(X),misma_familias(X,Y).
animales_vertebrados(X,Y):-animales(Z,Y),especies(X,Z).
relacionados(X,Y):-animales(Z,Y),parientes(X,Z).
relacion(X,Y):-animales(Z,X),misma_familias(Z,Y),animal(X).

%Estructuras
poseen(vertebrados,caracteristicas(esqueleto_interno,extremidades,cabeza,cuerpo,diferenciacion_sexual)).
tienen(mamiferos,caracteristicas(pelaje_o_pelos,sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades,se_alimentan_de_plantas_o_carnes)).
clasificacion(mamiferos(herbivoros,caracteristicas(pelaje,sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades_patas,se_alimentan_de_plantas))).
clasificacion(mamiferos(carnivoros,caracteristicas(pelaje,sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades,se_alimentan_de_carnes))).
clasificacion(mamiferos(murcielagos,caracteristicas(sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades_tanto_alas_como_patas,se_alimentan_de_frutas))).
clasificacion(mamiferos(cetaceos,caracteristicas(nadan,sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades_aletas,se_alimentan_de_peces))).
clasificacion(mamiferos(roedores,caracteristicas(sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades_patas,se_alimentan_de_semillas))).
clasificacion(mamiferos(marsupiales,caracteristicas(sangre_caliente,viviparos,respiran_por_los_pulmones,glandulas_mamarias,extremidades_patas,se_alimentan_de_plantas))).
se_caracterizan(aves,caracteristicas(pico,vuelan,oviparos,respiran_por_los_pulmones,extremidades_alas,se_alimentan_de_plantas_o_semillas_o_peces)).
posee(peces,caracteristicas(cubierto_por_escamas,oviparos,respiran_por_las_branquias,extremidades_aletas,se_alimentan_de_peces)).
se_basan(reptiles,caracteristicas(cubierto_por_escamas,oviparos,respiran_por_los_pulmones,sangre_fria,herbivoros_o_carnivoros)).
clasificacion1(reptiles(lagartos,caracteristicas(cubierto_por_escamas,oviparos,respiran_por_los_pulmones,sangre_fria,herbivoros_o_carnivoros_o_insectivoros))).
clasificacion2(reptiles(serpientes,caracteristicas(cubierto_por_escamas,oviparos,respiran_por_los_pulmones,sangre_fria,insectivoros))).
clasificacion3(reptiles(tortugas,caracteristicas(oviparos,respiran_por_los_pulmones,sangre_fria,herbivoros))).
poseen2(anfibios,caracteristicas(no_tienen_cola,oviparos,respiran_por_los_pulmones_o_branquias,sangre_fria,insectivoros)).

%LISTAS
herbivoros([ovejas,cabras,alces,ciervos,camellos]).
carnivoros([lobos,leones,mapaches,zorros,perros]).
murcielagos([murcielagos_blanco,murcielagos_pata_peluda,murcielagos_lengueton,murcielagos_trompudo]).
cetaceos([ballenas,delfines,orcas,cachalotes]).
roedores([ratas,ardillas,castores,capibaras]).
marsupiales([canguros,koala,numbat]).
aves([avestruz,kiwis,gallinas,patos,gaviotas]).
peces([pez_globo,pez_loro,pez_payaso,pez_martillo,sardina]).
lagartos([camaleon,gorgona,anolis,caiman,cocodrilo]).
serpientes([cobra,mamba_negra,cascabel,piton]).
tortugas([tortuga_laud,tortuga_verde,tortuga_carey,tortuga_bastarda]).
anfibios([ranas,sapos,triton,salamandras,gallipato]).

%adicionar
adicionar(X,L,[X|L]).
%eliminar
eliminar(X, [X|Cola],Cola).
%alternar
eliminar(X,[ Y |Cola],[ Y |Cola1]):-eliminar(X,Cola,Cola1).

%RECURSIVIDAD 
masgrande( ballenas, cachalote).
masgrande( cachalote, orcas).
masgrande( orcas, alces).
masgrande( alces, camellos).
masgrande( camellos, leones).
masgrande( leones, lobos).
masgrande( lobos, delfines).
masgrande( delfines, ovejas).
masgrande( ovejas, ciervos).
masgrande( ciervos, cabras).
masgrande( camellos, ovejas).
masgrande( ovejas,perros).
masgrande( perros,capibaras ).
masgrande( capibaras,zorros ).
masgrande(zorros ,mapaches ).
masgrande(mapaches, castores).
masgrande(castores, ardillas).
masgrande(ardillas,murcielagos_blanco).
masgrande(murcielagos_blanco,murcielagos_pata_peluda).
masgrande(murcielagos_pata_peluda,murcielagos_lengueton).
masgrande(murcielagos_lengueton,murcielagos_trompudos).
masgrande(murcielagos_lengueton,ratas).

muchomasgrande(A,B) :- masgrande(A,B).
muchomasgrande(A,B) :- masgrande(A,X),muchomasgrande(X,B).


%CONCATENACION.

%formar parejas
concatenar_animales(X,P) :-
    herbivoros(Herb),
    carnivoros(Carn),
    nth0(I,Herb,X),
    nth0(I,Carn,P).

concatenar_animales2(X,P) :-
    murcielagos(Murc),
    cetaceos(Cet),
    nth0(I,Murc,X),
    nth0(I,Cet,P).

concatenar_animales3(X,P) :-
    roedores(Roe),
    marsupiales(Mar),
    nth0(I,Roe,X),
    nth0(I,Mar,P).

concatenar_animales4(X,P) :-
    aves(Aves),
   peces(Pec),
    nth0(I,Aves,X),
    nth0(I,Pec,P).

%UNIR LISTAS
concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):-concatenar(L1,L2,L3).

