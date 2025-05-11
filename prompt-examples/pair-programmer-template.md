<config model="o3"  max_tokens="100000" reasoning_effort="high"/>

<tool name="apply_patch" />
<tool name="read_dir" />
<tool name="get_contents" />
<tool name="dune" command="dune" description="Use to run dune commands on on ocaml project. Dune build returns nothing if successful" />


<msg role="developer">
 <doc src="./prompt-examples/coding_assistant.txt" local/>
</msg>