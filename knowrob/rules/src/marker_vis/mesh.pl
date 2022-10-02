:- module(oe_mesh_vis,
	[ ]).

:- use_module(library(openease)).

%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_objects(ResultSet, Objs),
	member(Obj, Objs),
	object_shape(Obj, _, mesh(MeshPath,_), _, _),
	!, % TODO: support to show multiple meshes
	% publish the message,
        data_vis:data_vis_object(type(QueryID,988), Msg),
        data_vis:data_vis_set_property(Msg, title:MeshPath),
        data_vis:data_vis_publish(Msg).

