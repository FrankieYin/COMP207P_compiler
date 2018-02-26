main {
	seq<int> a := [1, 2, 3]; 
	seq<int> b := [4, 5, 6]; 
	int i := 0;
	int j := 0;
	while (i <= 2) do
		while (j <= 2) do
			if (b[j] < a[i]) then
				break 2; # break out of two loops
			fi
			j := j + 1;
		od
		i := i + 1;
		j := 0;
	od
};