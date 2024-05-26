(* ::Package:: *)

(* ::Section::Initialization:: *)
(*acoustic 2 sites*)


(* ::Input::Initialization:: *)
(*boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Table[Graphics[{Black,Rectangle[{i,0}+{0,-.2},{3,.2}]}],{i,0,30,3}]];
boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Graphics[{Black,Rectangle[{0,-.2},{30,.2}]}]];*)raio=1.4;
espessura1=.4;
espessura2=1.;

espessura1=.6;
espessura2=.3;

separacao=3;
sites=2

boundary=Show[Table[Graphics[Disk[{i,0},raio]],{i,0,(sites-1)*separacao,separacao}],Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{.9raio,-espessura1/2},{i,0}+{separacao-.9 raio,espessura1/2}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2},{i,0}+{separacao,espessura2/2}]}]],{i,0,(sites-2)*separacao,separacao}],ImageSize->600]


(* ::Input::Initialization:: *)
bdr=BoundaryDiscretizeGraphics[boundary,MaxCellMeasure->{"Area"->0.01}, AccuracyGoal->20,PrecisionGoal->20,MeshCellStyle->{{1,All}->Red,{0,All}->Black}]


(* ::Input::Initialization:: *)
{vals,funs}=NDEigensystem[{-Laplacian[u[x,y],{x,y}],DirichletCondition[u[x,y]==0,True]},u[x,y],{x,y}\[Element]bdr,sites,Method->{"PDEDiscretization"->{"FiniteElement","MeshOptions"->{"Area"->0.01}}}];


(* ::Input::Initialization:: *)
vals
ListPlot[vals]


(* ::Input::Initialization:: *)
Table[Plot3D[funs[[i]],{x,y}\[Element]bdr,PlotRange->All,PlotLabel->vals[[i]],PlotTheme->"Minimal",BoxRatios->{sites,1,3}],{i,Length[vals]}]


(* ::Input::Initialization:: *)
Column[{Table["harmonico  "<>ToString[i],{i,1,sites}],Table["energy= "<>ToString[vals[[i]]],{i,1,sites}],Table[Plot[funs[[i]]/.{x->xvalue,y->0},{xvalue,-raio,(sites-1)*separacao+raio},PlotRange->All],{i,1,sites}]}//Transpose]


(* ::Section::Initialization:: *)
(*acoustic circles*)


(* ::Input::Initialization:: *)
(*boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Table[Graphics[{Black,Rectangle[{i,0}+{0,-.2},{3,.2}]}],{i,0,30,3}]];
boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Graphics[{Black,Rectangle[{0,-.2},{30,.2}]}]];*)
raio=1.499;
espessura1=.4;
espessura2=1.;

espessura1=1.;
espessura2=0.6;

separacao=4;
sites=12;

boundary=Show[Table[Graphics[Disk[{i,0},raio]],{i,0,(sites-1)*separacao,separacao}],Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{0,-espessura1/2},{i,0}+{separacao,espessura1/2}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2},{i,0}+{separacao,espessura2/2}]}]],{i,0,(sites-2)*separacao,separacao}],ImageSize->600]


(* ::Input::Initialization:: *)
bdr=BoundaryDiscretizeGraphics[boundary,MaxCellMeasure->{"Area"->0.01}, AccuracyGoal->20,PrecisionGoal->20,MeshCellStyle->{{1,All}->Red,{0,All}->Black},ImageSize->1000]


(* ::Input::Initialization:: *)



(* ::Input::Initialization:: *)
{vals,funs}=NDEigensystem[{-Laplacian[u[x,y],{x,y}],DirichletCondition[u[x,y]==0,True]},u[x,y],{x,y}\[Element]bdr,sites,Method->{"PDEDiscretization"->{"FiniteElement","MeshOptions"->{"MaxCellMeasure"->0.001}}}];


(* ::Input::Initialization:: *)
vals
ListPlot[vals]


(* ::Input::Initialization:: *)
Table[Plot3D[funs[[i]],{x,y}\[Element]bdr,PlotRange->All,PlotLabel->vals[[i]],PlotTheme->"Minimal",BoxRatios->{10,1,3}],{i,Length[vals]}]


(* ::Input::Initialization:: *)
Column[{Table["harmonico  "<>ToString[i],{i,1,sites}],Table["energy= "<>ToString[vals[[i]]],{i,1,sites}],Table[Plot[funs[[i]]/.{x->xvalue,y->0},{xvalue,-raio,(sites-1)*separacao+raio},PlotRange->All],{i,1,sites}]}//Transpose]


(* ::Section::Initialization:: *)
(*acoustic rectangles*)


(* ::Input::Initialization:: *)
(*boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Table[Graphics[{Black,Rectangle[{i,0}+{0,-.2},{3,.2}]}],{i,0,30,3}]];
boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Graphics[{Black,Rectangle[{0,-.2},{30,.2}]}]];*)
raio=1.;
espessura1=.4;
espessura2=1.;

espessura1=1;
espessura2=.8;
altura=10;
largura=2 raio;

separacao=3;
sites=12;

boundary=Show[Table[Graphics[{Black,Rectangle[{i,0}+{-largura/2,-altura/2},{i,0}+{largura/2,altura/2}]}],{i,0,(sites-1)*separacao,separacao}],Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{0,-espessura1/2},{i,0}+{separacao,espessura1/2}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2},{i,0}+{separacao,espessura2/2}]}]],{i,0,(sites-2)*separacao,separacao}],ImageSize->600]


(* ::Input::Initialization:: *)
bdr=BoundaryDiscretizeGraphics[boundary,MaxCellMeasure->{"Area"->0.01}, AccuracyGoal->20,PrecisionGoal->20,MeshCellStyle->{{1,All}->Red,{0,All}->Black},ImageSize->1000]


(* ::Input::Initialization:: *)



(* ::Input::Initialization:: *)
{vals,funs}=NDEigensystem[{-Laplacian[u[x,y],{x,y}],DirichletCondition[u[x,y]==0,True]},u[x,y],{x,y}\[Element]bdr,2 sites,Method->{"PDEDiscretization"->{"FiniteElement","MeshOptions"->{"MaxCellMeasure"->0.01}}}];


(* ::Input::Initialization:: *)
vals
ListPlot[vals]


(* ::Input::Initialization:: *)
Table[Plot3D[funs[[i]],{x,y}\[Element]bdr,PlotRange->All,PlotLabel->vals[[i]],PlotTheme->"Minimal",BoxRatios->{10,1,3}],{i,Length[vals]}]


(* ::Input::Initialization:: *)
Column[{Table["harmonico  "<>ToString[i],{i,1,sites}],Table["energy= "<>ToString[vals[[i]]],{i,1,sites}],Table[Plot[funs[[i]]/.{x->xvalue,y->0},{xvalue,-raio,(sites-1)*separacao+raio},PlotRange->All],{i,1,sites}]}//Transpose]


(* ::Section::Initialization:: *)
(*acoustic rectangles two channels*)


(* ::Input::Initialization:: *)
(*boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Table[Graphics[{Black,Rectangle[{i,0}+{0,-.2},{3,.2}]}],{i,0,30,3}]];
boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Graphics[{Black,Rectangle[{0,-.2},{30,.2}]}]];*)
raio=1.;
espessura1=.4;
espessura2=1.;

espessura1=1;
espessura2=.8;
altura=10;
largura=2 raio;

separacao=3;
sites=12;

boundary=Show[
Table[Graphics[{Black,Rectangle[{i,0}+{-largura/2,-altura/2},{i,0}+{largura/2,altura/2}]}],{i,0,(sites-1)*separacao,separacao}],Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{0,-espessura1/2+ altura/4},{i,0}+{separacao,espessura1/2+altura/4}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2+altura/4},{i,0}+{separacao,espessura2/2+altura/4}]}]],{i,0,(sites-2)*separacao,separacao}],
Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{0,-espessura1/2-altura/4},{i,0}+{separacao,espessura1/2-altura/4}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2-altura/4},{i,0}+{separacao,espessura2/2-altura/4}]}]],{i,0,(sites-2)*separacao,separacao}],ImageSize->600]


(* ::Input::Initialization:: *)
bdr=BoundaryDiscretizeGraphics[boundary,MaxCellMeasure->{"Area"->0.01}, AccuracyGoal->20,PrecisionGoal->20,MeshCellStyle->{{1,All}->Red,{0,All}->Black},ImageSize->1000]


(* ::Input::Initialization:: *)



(* ::Input::Initialization:: *)
{vals,funs}=NDEigensystem[{-Laplacian[u[x,y],{x,y}],DirichletCondition[u[x,y]==0,True]},u[x,y],{x,y}\[Element]bdr,2 sites,Method->{"PDEDiscretization"->{"FiniteElement","MeshOptions"->{"MaxCellMeasure"->0.01}}}];


(* ::Input::Initialization:: *)
vals
ListPlot[vals]


(* ::Input::Initialization:: *)
Table[Plot3D[funs[[i]],{x,y}\[Element]bdr,PlotRange->All,PlotLabel->vals[[i]],PlotTheme->"Minimal",BoxRatios->{10,1,3}],{i,Length[vals]}]


(* ::Input::Initialization:: *)
Column[{Table["harmonico  "<>ToString[i],{i,1,2 sites}],Table["energy= "<>ToString[vals[[i]]],{i,1,2 sites}],Table[Plot[funs[[i]]/.{x->xvalue,y->0},{xvalue,-raio,(sites-1)*separacao+raio},PlotRange->All],{i,1,2 sites}]}//Transpose]


(* ::Section::Initialization:: *)
(*acoustic rectangles two channels remove dirichlet*)


(* ::Input::Initialization:: *)
(*boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Table[Graphics[{Black,Rectangle[{i,0}+{0,-.2},{3,.2}]}],{i,0,30,3}]];
boundary=Show[Table[Graphics[Disk[{i,0}]],{i,0,30,3}],Graphics[{Black,Rectangle[{0,-.2},{30,.2}]}]];*)
raio=1.;
espessura1=.4;
espessura2=1.;

espessura1=.8;
espessura2=1.;
altura=10;
largura=2 raio;

separacao=3;
sites=12;

boundary=Show[
Table[Graphics[{Black,Rectangle[{i,0}+{-largura/2,-altura/2},{i,0}+{largura/2,altura/2}]}],{i,0,(sites-1)*separacao,separacao}],Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{0,-espessura1/2+ altura/4},{i,0}+{separacao,espessura1/2+altura/4}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2+altura/4},{i,0}+{separacao,espessura2/2+altura/4}]}]],{i,0,(sites-2)*separacao,separacao}],
Table[If[EvenQ[i/separacao],Graphics[{Black,Rectangle[{i,0}+{0,-espessura1/2-altura/4},{i,0}+{separacao,espessura1/2-altura/4}]}],Graphics[{Black,Rectangle[{i,0}+{0,-espessura2/2-altura/4},{i,0}+{separacao,espessura2/2-altura/4}]}]],{i,0,(sites-2)*separacao,separacao}],ImageSize->600]


(* ::Input::Initialization:: *)
bdr=BoundaryDiscretizeGraphics[boundary,MaxCellMeasure->{"Area"->0.01}, AccuracyGoal->20,PrecisionGoal->20,MeshCellStyle->{{1,All}->Red,{0,All}->Black},ImageSize->1000]


(* ::Input::Initialization:: *)



(* ::Input::Initialization:: *)
{vals,funs}=NDEigensystem[{-Laplacian[u[x,y],{x,y}]},u[x,y],{x,y}\[Element]bdr,2 sites,Method->{"PDEDiscretization"->{"FiniteElement","MeshOptions"->{"MaxCellMeasure"->0.01}}}];


(* ::Input::Initialization:: *)
vals
ListPlot[vals]


(* ::Input::Initialization:: *)
Table[Plot3D[funs[[i]],{x,y}\[Element]bdr,PlotRange->All,PlotLabel->vals[[i]],PlotTheme->"Minimal",BoxRatios->{10,1,3}],{i,Length[vals]}]


(* ::Input::Initialization:: *)
Column[{Table["harmonico  "<>ToString[i],{i,1,2 sites}],Table["energy= "<>ToString[vals[[i]]],{i,1,2 sites}],Table[Plot[funs[[i]]/.{x->xvalue,y->0},{xvalue,-raio,(sites-1)*separacao+raio},PlotRange->All],{i,1,2 sites}]}//Transpose]
