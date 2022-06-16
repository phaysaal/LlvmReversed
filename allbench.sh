while read p; do
		echo "To be VERIFIED $p"
		./llvmrev "$p"
done <bench.txt 2>/dev/null | grep "VER"
