:- module(oe_mesh_vis,
	[ ]).

:- use_module(library(openease)).

%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_objects(ResultSet, Objs),
	member(Obj, Objs),
	% FIXME: get it right here. are the pathToCADModel value constraints needed at all?
	%object_shape(Obj, _, mesh(MeshPath,_), _, _),
	instance_of_description(Obj, value('http://knowrob.org/kb/knowrob.owl#pathToCadModel',MeshPath)),
	
	% XXX: remove HACK
	(  atom_concat('package://iai_maps/iai_apartment',Rest,MeshPath)
	-> atom_concat('package://iai_apartment',Rest,MeshPath2)
	;  MeshPath2=MeshPath
	),
	
	!, % TODO: support to show multiple meshes
	% publish the message,
        data_vis:data_vis_object(type(QueryID,988), Msg),
        data_vis:data_vis_set_property(Msg, title:MeshPath2),
        data_vis:data_vis_publish(Msg).

