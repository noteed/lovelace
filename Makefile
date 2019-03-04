/home/thu/projects/entrypoint/as-is/projects/lovelace/example.svg: \
	example.svg \
	example-1.svg \
	example-2.svg \
	example-3.svg \
	example-4.svg \
	example-5.svg \
	example-6.svg \
	example-7.svg \
	identity.svg \
	identity.log \
	success-task.svg \
	success-task.log
	cp *.svg *.log /home/thu/projects/entrypoint/as-is/projects/lovelace/

%.svg: %.dot
	nix-shell -p graphviz --run 'dot -Tsvg -o $@ $<'

example.dot: Lovelace.hs bin/lovelace.hs
	nix-shell --pure --run 'runghc bin/lovelace.hs graph'

identity.dot identity.log: Lovelace.hs bin/lovelace.hs tests/Identity.hs
	nix-shell --pure --run 'runghc -itests/ tests/Identity.hs hello'

success-task.dot success-task.log: Lovelace.hs bin/lovelace.hs tests/SuccessTask.hs
	nix-shell --pure --run 'runghc -itests/ tests/SuccessTask.hs hello'
