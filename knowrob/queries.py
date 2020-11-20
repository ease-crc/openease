
# 'object_shape(Obj,Shape,Origin,Material).'
# 'object_color_rgb(Obj,RGB).'
# 'has_component(Obj,Component).'
# 'has_sensor(Obj,Sensor).'
# 'has_feature(Obj,Feature).'
# 'is_at(Obj,Pose).'
# 'has link(Joint,Obj).'
# 'has joint position limit(Joint,Limit).'
# 'has_mass(Obj,Mass)).'
# 'has_net_force(Obj,Force).'
# 'affords(Obj,Affordance).'
# 'is_event(Evt).'
# 'is_action(Evt).'
# 'has_participant(Evt,Obj).'
# 'is_performed_by(Evt,Agent).'
# 'has_phase(Evt,Phase).'
# 'has_motion_phase(Evt,Phase).'
# 'has_transformation(Evt,From,To).'
# 'has_event_type(Evt,EvtType).'
# 'executes_task(Evt,Tsk).'
# 'is_failed_action(Act).'
# 'is_setting_for(Episode,Entity).'
# 'satisfies_plan(Episode,Plan).'

EXAMPLES = [
    {
        'name': 'Activity knowledge',
        'sub_sections': [
            {
                'id': 'act_occ',
                'icon': 'fa-car-crash',
                'name': 'Occurrences',
                'queries': []
            },
            {
                'id': 'act_parti',
                'icon': 'fa-people-carry',
                'name': 'Participation',
                'queries': []
            },
            {
                'id': 'act_struct',
                'icon': 'fa-sitemap',
                'name': 'Structure',
                'queries': []
            },
            {
                'id': 'act_trans',
                'icon': 'fa-random',
                'name': 'Transformation',
                'queries': []
            },
            {
                'id': 'act_concept',
                'icon': 'fa-expand',
                'name': 'Conceptualization',
                'queries': []
            },
            {
                'id': 'act_context',
                'icon': 'fa-expand',
                'name': 'Contextualization',
                'queries': []
            }
        ]
    },
    {
        'name': 'Environment knowledge',
        'sub_sections': [
            {
                'id': 'env_app',
                'icon': 'fa-shapes',
                'name': 'Appearance',
                'queries': []
            },
            {
                'id': 'env_struct',
                'icon': 'fa-sitemap',
                'name': 'Structure',
                'queries': []
            },
            {
                'id': 'env_kin',
                'icon': 'fa-running',
                'name': 'Kinematics',
                'queries': []
            },
            {
                'id': 'env_dyn',
                'icon': 'fa-fill-drip',
                'name': 'Dynamics',
                'queries': []
            },
            {
                'id': 'env_phy',
                'icon': 'fa-utensils',
                'name': 'Naive physics',
                'queries': []
            }
        ]
    },
    {
        'name': 'Agent knowledge',
        'sub_sections': [
            {
                'id': 'ag_struc',
                'icon': 'fa-sitemap',
                'name': 'Structure',
                'queries': []
            },
            {
                'id': 'ag_kin',
                'icon': 'fa-running',
                'name': 'Kinematics',
                'queries': []
            }
        ]
    }
]
