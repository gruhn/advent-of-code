let [a,b,c,d,e,f,g,h] = [1,0,0,0,0,0,0,0]

// set b 99
b = 99

// set c b
c = b

// jnz a 2
// jnz 1 5
if (a !== 0) {
	// mul b 100
	// sub b -100000
	b = b*100 + 100000
	// set c b
	// sub c -17000
	c = b + 17000
}

do {
	// set f 1
	f = 1
	// set d 2
	d = 2

	do {
		// set e 2
		e = 2

		/*
		do {
			// set g d
			// mul g e
			// sub g b
			g = d * e - b

			// jnz g 2
			if (g === 0) {
				// set f 0 
				f = 0
			}

			// sub e -1
			e = e + 1

			// set g e
			// sub g b
			g = e - b

			// jnz g -8
		} while (g !== 0)
		*/

		if (b >= 2*d && b % d === 0) {
			f = 0
		}
		e = b
		g = 0

		// sub d -1
		d = d + 1
		// set g d
		// sub g b
		g = d - b
		// jnz g -13
	} while (g !== 0)

	// jnz f 2
	if (f === 0) {
		// sub h -1
		h = h+1
	}

	// set g b
	// sub g c
	g = b - c

	// jnz g 2
	// jnz 1 3
	if (g !== 0) {
		// sub b -17
		b = b + 17
	}
	// jnz 1 -23
} while (g !== 0)

console.log(a,b,c,d,e,f,g,h)
