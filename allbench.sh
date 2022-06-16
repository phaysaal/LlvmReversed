while read p; do
		echo "To be Verified $p"
		./llvmrev "$p"
done <bench.txt 2>/dev/null | grep "Veri"
