while read p; do
		echo "To be VERIfied $p"
		./llvmrev "$p"
done <bench.txt 2>/dev/null | grep "VERI"
