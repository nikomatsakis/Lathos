
%% id: Node identifiers.
-record(id, { name, version }).

%% node: Node structures.
%%   id: #id
%%   parent_ids: [#id]
%%   description: [{text, <<...>>} | {link, <<...>>, Id}]
-record(node, { id, parent_ids, description }).
