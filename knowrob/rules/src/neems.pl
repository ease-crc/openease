:- module(neems,
	[ neem_init/1
	]).

neem_init(NEEM_id) :-
	% assign DB collection prefix
	set_setting(mng_client:collection_prefix, NEEM_id),
	% load URDF files referred to in triple store
	urdf_init,
	% initialize position of each frame for tf publishing
	tf_tree:initial_transforms(InitialTransforms),
	forall(
	    (   member([Ref,Frame,Pos,Rot],InitialTransforms),
	        % FIXME avoid this elsewhere
	        Ref \= Frame,
	        \+ atom_concat('/',Ref,Frame),
	        \+ atom_concat('/',Frame,Ref)
	    ),
		tf_plugin:tf_republish_set_pose(Frame,[Ref,Pos,Rot])
	),
	% publish object marker messages
	marker_plugin:republish.
