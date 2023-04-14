(* ::Package:: *)

BeginPackage["Pixelization`"];


Triangulate::"\:0422\:0440\:0438\:0430\:043d\:0433\:0443\:043b\:044f\:0446\:0438\:044f \:0438\:0437\:043e\:0431\:0440\:0430\:0436\:0435\:043d\:0438\:044f \:0440\:0430\:0432\:043d\:043e\:0441\:0442\:043e\:0440\:043e\:043d\:043d\:0438\:043c\:0438 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430\:043c\:0438";
TriangulateHex::"\:0410\:043b\:044c\:0442\:0435\:0440\:043d\:0430\:0442\:0438\:0432\:043d\:0430\:044f \:0442\:0440\:0438\:0430\:043d\:0433\:0443\:043b\:044f\:0446\:0438\:044f \:0440\:0430\:0432\:043d\:043e\:0441\:0442\:043e\:0440\:043e\:043d\:043d\:0438\:043c\:0438 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430\:043c\:0438";
IrregularTriangulation::"\:041d\:0435\:0440\:0435\:0433\:0443\:043b\:044f\:0440\:043d\:043e\:0435 \:0440\:0430\:0437\:0431\:0438\:0435\:043d\:0438\:0435 \:043d\:0430 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0438";
Hexagonize::"\:0413\:0435\:043a\:0441\:0430\:0433\:043e\:043d\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f";
Pixelate::"\:041f\:0438\:043a\:0441\:0435\:043b\:0438\:0437\:0430\:0446\:0438\:044f";
PixelateTr::"\:0422\:0440\:0438\:0430\:043d\:0433\:0443\:043b\:044f\:0446\:0438\:044f \:043f\:0440\:044f\:043c\:043e\:0443\:0433\:043e\:043b\:044c\:043d\:044b\:043c\:0438 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430\:043c\:0438";
Rombize::"\:0420\:043e\:043c\:0431\:044b \:043d\:0430 \:043e\:0441\:043d\:043e\:0432\:0435 \:0448\:0435\:0441\:0442\:0438\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:043e\:0432. \:0414\:0430\:044e\:0442 \:044d\:0444\:0444\:0435\:043a\:0442 3D-\:043a\:0443\:0431\:0438\:043a\:043e\:0432";
Voronize::"\:0420\:0430\:0437\:0431\:0438\:0435\:043d\:0438\:0435 \:0412\:043e\:0440\:043e\:043d\:043e\:0433\:043e";
ToBricks::"\:041a\:0438\:0440\:043f\:0438\:0447\:0438\:0437\:0430\:0446\:0438\:044f";
TriangulationPoints::"";



Get[(NotebookDirectory[]<>"\\Modules\\DiscretizationRoutines.m")];


Begin["`Private`"];


iSteps[region_?RegionQ,pointshorizcount_,steps:{hstep_,vstep_}]:=(ObjectDimensions[region][[1]]/pointshorizcount)steps

Steps[region_?RegionQ,pointshorizcount_,"Tr"]:=iSteps[region,pointshorizcount,{1,Tan[30\[Degree]]}](*\:0438\:0441\:043f\:043e\:043b\:044c\:0437\:0443\:0435\:0442\:0441\:044f \:0434\:043b\:044f Triangulate*) 
Steps[region_?RegionQ,pointshorizcount_,"TrHex"]:=iSteps[region,pointshorizcount,{1,2Cos[30\[Degree]]}](*\:0438\:0441\:043f\:043e\:043b\:044c\:0437\:0443\:0435\:0442\:0441\:044f \:0434\:043b\:044f TriangulateHex \:0438 Hexagonize *)

TriangulationPoints[region_?RegionQ,pointshorizcount_,destin_String]:=Module[{steps=Steps[region,pointshorizcount,destin],pointsvertcount},
pointsvertcount=Round[ObjectDimensions[region][[2]]/steps[[2]]];
(Flatten[Table[region[[1]]+steps{i,j},{i,0,pointshorizcount,1},{j,0,pointsvertcount,1}],1]~Join~Flatten[Table[region[[1]]+0.5steps+steps{i,j},{i,0,pointshorizcount-1,1},{j,0,pointsvertcount-1,1}],1])]


iTriangulate[region_?RegionQ,pointshorizcount_,destin_String]:=DelaunayMesh@TriangulationPoints[region,pointshorizcount,destin]
iTriangulate[image_?ImageQ,pointshorizcount_,destin_String]:=ToPolygons[image,iTriangulate[Rectangle[{0,0},ImageDimensions[image]],pointshorizcount,destin]]


Triangulate[image_?ImageQ,pointshorizcount_]:=iTriangulate[image,pointshorizcount,"Tr"]
TriangulateHex[image_?ImageQ,pointshorizcount_]:=iTriangulate[image,pointshorizcount,"TrHex"]


HexagonPointsCoordinates[center:{x_,y_},radius_]:=center+RotationMatrix[# \[Degree]].(radius{Cos[30\[Degree]],Sin[30\[Degree]]})&/@{0,60,120,180,240,300}
HexagonPointsCoordinatesWithCenter[center:{x_,y_},radius_]:={center}~Join~HexagonPointsCoordinates[center,radius] (*\:0434\:043b\:044f Rombize*)


HexagonRadius[region_?RegionQ,pointshorizcount_]:=Norm[Steps[region,pointshorizcount,"TrHex"]]/(2Sqrt[3])


Hexagonize[region_?RegionQ,pointshorizcount_]:=Module[{hexpts,centers=TriangulationPoints[region,pointshorizcount,"TrHex"]},
hexpts=Flatten[HexagonPointsCoordinates[#,HexagonRadius[region,pointshorizcount]]&/@centers,1];
MeshRegion[hexpts,Polygon[Partition[Range[Length[hexpts]],6]]]
]
Hexagonize[image_?ImageQ,pointshorizcount_]:=ToPolygons[image,Hexagonize[Rectangle[{0,0},ImageDimensions[image]],pointshorizcount]]



(*\:043f\:0438\:043a\:0441\:0435\:043b\:0438\:0437\:0430\:0446\:0438\:044f*)
SquareSide[image_?ImageQ,horizcount_Integer/;horizcount>0]:=N[ObjectDimensions[image][[1]]/horizcount]

SquaresCount[image_?ImageQ,horizcount_Integer/;horizcount>0]:=Round[ObjectDimensions[image]/SquareSide[image,horizcount]]

MeshPointsCount[image_?ImageQ,horizcount_Integer/;horizcount>0]:=With[{pcount=SquaresCount[image,horizcount]+1},{pcount[[1]],pcount[[2]],Times@@pcount}]

MeshPoints[image_?ImageQ,horizcount_Integer/;horizcount>0]:=With[{pcount=SquaresCount[image,horizcount]},MapIndexed[{#1,First@#2}&,Flatten[Table[SquareSide[image,horizcount]{i,j},{j,0,pcount[[2]]},{i,0,pcount[[1]]}],1]]]

SquareLeftBottomCorners[image_?ImageQ,horizcount_Integer/;horizcount>0]:=With[{pts=MeshPoints[image,horizcount]},Select[pts,#[[1,1]]<Last[pts][[1,1]]&&#[[1,2]]<Last[pts][[1,2]]&]]

NeighborsNumbers[image_?ImageQ,horizcount_Integer/;horizcount>0,num_Integer]:=With[{pcount=Most[MeshPointsCount[image,horizcount]]},{num,num+1,num+pcount[[1]]+1,num+pcount[[1]]}]
NeighborsNumbers[image_?ImageQ,horizcount_Integer/;horizcount>0]:=NeighborsNumbers[image,horizcount,#]&/@SquareLeftBottomCorners[image,horizcount][[All,2]]

SquareMesh[image_?ImageQ,horizcount_Integer/;horizcount>0]:=MeshRegion[MeshPoints[image,horizcount][[All,1]],Polygon[NeighborsNumbers[image,horizcount]]]

Pixelate[image_?ImageQ,horizcount_Integer/;horizcount>0]:=ToPolygons[image,SquareMesh[image,horizcount]]

(*\:043f\:0440\:044f\:043c\:043e\:0443\:0433\:043e\:043b\:044c\:043d\:044b\:0435 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0438*)
NeighborsNumbersTr[image_?ImageQ,horizcount_Integer/;horizcount>0,num_Integer]:=With[{pcount=Most[MeshPointsCount[image,horizcount]]},{{num,num+1,num+pcount[[1]]+1},{num,num+pcount[[1]],num+pcount[[1]]+1}}]
NeighborsNumbersTr[image_?ImageQ,horizcount_Integer/;horizcount>0]:=Flatten[NeighborsNumbersTr[image,horizcount,#]&/@SquareLeftBottomCorners[image,horizcount][[All,2]],1]

SquareMeshTr[image_?ImageQ,horizcount_Integer/;horizcount>0]:=MeshRegion[MeshPoints[image,horizcount][[All,1]],Polygon[NeighborsNumbersTr[image,horizcount]]]

PixelateTr[image_?ImageQ,horizcount_Integer/;horizcount>0]:=ToPolygons[image,SquareMeshTr[image,horizcount]]

(*\:0440\:043e\:043c\:0431\:044b/\:043a\:0443\:0431\:0438\:043a\:0438*)
Rombize[region_?RegionQ,pointshorizcount_]:=Module[{hexpts,centers=TriangulationPoints[region,pointshorizcount,"TrHex"]},
hexpts=Flatten[HexagonPointsCoordinatesWithCenter[#,HexagonRadius[region,pointshorizcount]]&/@centers,1];
MeshRegion[hexpts,Polygon[Flatten[{{#[[1]],#[[3]],#[[2]],#[[7]]},{#[[1]],#[[7]],#[[6]],#[[5]]},{#[[1]],#[[5]],#[[4]],#[[3]]}}&/@Partition[Range[Length[hexpts]],7],1]]]]

Rombize[image_?ImageQ,pointshorizcount_]:=ToPolygons[image,Rombize[Rectangle[{0,0},ImageDimensions[image]],pointshorizcount]]

(*\:043d\:0435\:0440\:0435\:0433\:0443\:043b\:044f\:0440\:043d\:0430\:044f \:0442\:0440\:0438\:0430\:043d\:0433\:0443\:043b\:044f\:0446\:0438\:044f*)
IrregularTriangulation[region_?RegionQ,pointshorizcount_]:=DiscretizeRegion[region,MaxCellMeasure->Max[ObjectDimensions[region]]/(0.05pointshorizcount)]
IrregularTriangulation[image_?ImageQ,pointshorizcount_]:=ToPolygons[image,IrregularTriangulation[Rectangle[{0,0},ImageDimensions[image]],pointshorizcount]]


(*\:0432\:043e\:0440\:043e\:043d\:0438\:0437\:0430\:0446\:0438\:044f*)
ObjectBounds[image_?(ImageQ[#]||RegionQ[#]&)]:=Transpose[{{0,0},ObjectDimensions[image]}]

EdgesCoords[image_?ImageQ,radius_]:=ImageValuePositions[EdgeDetect[image,radius],White]

FirstVoronoi::argerror="No edges detected";
FirstVoronoi::argerror1="Input error";
FirstVoronoi[edgespointscoords:{{_,_}..},imagebounds_]:=VoronoiMesh[edgespointscoords,imagebounds]
FirstVoronoi[{},imagebounds_]:=(Message[FirstVoronoi::argerror];$Failed)
FirstVoronoi[___]:=(Message[FirstVoronoi::argerror1];$Failed)

SecondVoronoi[firstvoronoi_?RegionQ]:=VoronoiMesh[Mean@@@MeshPrimitives[firstvoronoi,2],ImageBounds[firstvoronoi]]
SecondVoronoi[___]:=$Failed

MyVoronoiMesh[image_?ImageQ,radius_,level_:2]:=Nest[SecondVoronoi,FirstVoronoi[EdgesCoords[image,radius],ImageBounds[image]],level]

MyResize[image_?ImageQ/;Max[ObjectDimensions[image]]>580]:=ImageResize[image,580]
MyResize[image_?ImageQ/;Max[ObjectDimensions[image]]<=580]:=image

Voronize[image_?ImageQ,polzunok_,level_:2]:=With[{imageResized=MyResize[image]},DiscretizationRoutines`ToPolygons[imageResized,MyVoronoiMesh[imageResized,-0.593polzunok+81.186(*\:043f\:043e\:0434\:0433\:043e\:043d\:043a\:0430 \:0440\:0430\:0434\:0438\:0443\:0441\:0430 \:0432 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0438\:0435 \:043f\:043e\:043b\:0437\:0443\:043d\:043a\:0443 5...120*),level]]]


(*\:043a\:0438\:0440\:043f\:0438\:0447\:0438\:0437\:0430\:0446\:0438\:044f*)
BrickPoints[center:{x_,y_},w_,h_]:=(center+#)&/@(0.5{{-w,-h},{-w,h},{w,h},{w,-h}})

GroupByPositions[list_,pred_]:=With[{trueList=list[[Select[Range@Length@list,pred]]]},{trueList,Complement[list,trueList]}]

MyOffset[pts:{{_,_}..},offset:{dx_,dy_}]:=offset+#&/@pts

BrickCenters[regiondimensions:{w_,h_},vcount_,brickcoeff_]:=Module[{brickheight=h/vcount,brickwidth,hcount,raw,offset},
brickwidth=brickcoeff brickheight;
hcount=w/brickwidth;
Composition[
Flatten[#,1]&,
MapAt[MyOffset[#,{-brickwidth/2,0}]&,#,2]&,(*\:043f\:0440\:0438\:043c\:0435\:043d\:044f\:0435\:043c \:043a \:0447\:0435\:0442\:043d\:044b\:043c \:0441\:0434\:0432\:0438\:0433*)
Flatten[#,1]&/@#&,
GroupByPositions[#,OddQ]&(*\:0440\:0430\:0437\:0434\:0435\:043b\:044f\:0435\:043c \:043d\:0435\:0447\:0435\:0442\:043d\:044b\:0435 \:0438 \:0447\:0435\:0442\:043d\:044b\:0435 \:0441\:0442\:0440\:043e\:043a\:0438*)
][Table[0.5{brickwidth,brickheight}+{brickwidth i,brickheight j},{j,0,vcount},{i,0,hcount+1}]]]

ToBricks[region_?RegionQ,vcount_,brickcoeff_]:=Module[{brickheight,brickwidth},
brickheight=ObjectDimensions[region][[2]]/vcount;
brickwidth=brickcoeff brickheight;
Composition[
MeshRegion[#,Polygon[Partition[Range[Length[#]],4]]]&,
Flatten[#,1]&,
Map[BrickPoints[#,brickwidth,brickheight]&,#]&,
BrickCenters[ObjectDimensions[#],vcount,brickcoeff]&]
[region]]

ToBricks[image_?ImageQ,vcount_,brickcoeff_]:=ToPolygons[image,ToBricks[Rectangle[{0,0},ImageDimensions[image]],vcount,brickcoeff]]


End[];


EndPackage[];
