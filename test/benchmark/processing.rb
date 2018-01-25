require'./config'

plotconfig = PLOTCONFIG.map{|k, v|
	sprintf("%s=%s", k, v)
}.join(",")


def stream_ignore_while stream, pat # {{{
	ignore = true
	stream.each_line{|line|
		line.chomp!
		ignore = ignore ^ line.match(pat)

		break unless ignore
	}
end # }}}

def process_benchresult stream # {{{
	bench_result = {}
	end_of_bench = false

	stream.each_line{|line|
		line.chomp!
		end_of_bench = line.match(/^\s*$/)

		break if end_of_bench

		results = line.match(/\s*(?<case>\S+)\s*\(\s*(?<lang>[a-z]+)\)\s*\(\s*(?<size>\d+)\)\s*(?<time>\d+\_?\d+)\.?\d+ms/)

		if results
			testcase = results[:case]

			if not bench_result[testcase]
				bench_result[testcase] = {}
				bench_result[testcase][results[:lang]] = []
			end

			if not bench_result[testcase][results[:lang]]
				bench_result[testcase][results[:lang]] = []
			end

			size = results[:size].to_i
			time0 = results[:time].gsub("_", "")

			time = time0.to_f * 0.001

			bench_result[testcase][results[:lang]].push [size, time]
		end
	}

	return bench_result
end # }}}

def process_codesize stream # {{{
	end_of_codesize = false
	size_result = {}

	stream.each_line{|line|
		line.chomp!
		end_of_codesize = line.match(/^%%%%%$/)

		break if end_of_codesize

		results = line.match(/\s*(?<case>\w+)\s*\|\s*(?<coreml>\d+)\s*\|\s*(?<joel>\d+)\s*\|\s*(?<cps>\d+)/)

		if results
			testcase = results[:case]

			if not size_result[testcase]
				size_result[testcase] = {}
			end

			c = size_result[testcase]

			c["coreml"] = results[:coreml].to_i
			c["joel"] = results[:joel].to_i
			c["cps"] = results[:cps].to_i
		end
	}

	return size_result
end # }}}

# map for tikz plot {{{
X_SCALE = 10.0
Y_SCALE = 1000.0
C_SCALE = 50.0

def mapto_xy xy
	x, y = xy

	return x / X_SCALE, y / Y_SCALE
end

def mapto_codesize size
	return size / C_SCALE
end
# }}}

# latex utils {{{
def latexenv envname, opt: [], &env
	if opt.class.to_s == "String"
		opt = [opt]
	end

	opt = (opt != []) ? (sprintf "[%s]", opt.join(",")) : ""

	printf "\\begin{%s}%s\n", envname, opt
	env.call
	printf "\\end{%s}\n\n", envname
end
# }}}

# main {{{
###
stream = STDIN

# ignore header
stream_ignore_while(stream, /%%%%%/)
stream_ignore_while(stream, /^\s*-+\s+-+\s*$/)
##
bench_result = process_benchresult stream
##
# ignore output
stream_ignore_while(stream, /^\s*-+\s*$/)
##
size_result = process_codesize stream
## ignore for EOF
###

# plot {{{
header = <<'HEAD'
\documentclass[dvipdfmx]{article}
\usepackage{luatex85}
\usepackage{pgfplots,tikz}
\usetikzlibrary{patterns, calc,positioning}

\tikzset{
    joel/.style =   {red,   },
    cps/.style =    {blue,  },
    coreml/.style = {green  },
}
HEAD

puts header

latexenv "document" do
	printf "\\newcommand{\\dotsize}{%f}\n", 0.1
	# printf "\\newcommand{\\xscale}{%f}\n", X_SCALE
	# printf "\\newcommand{\\yscale}{%f}\n", Y_SCALE

	bench_result.each{|testcase, conts|
		latexenv "figure", opt: "h" do
			puts "\\centering"
			latexenv "tikzpicture" do
				latexenv "axis", opt: plotconfig do
					conts.each{|lang, times|

						printf "\\addplot[%s, mark = *] coordinates {\n", lang
						times.each{|xy|
							x, y = xy
							printf "(%d, %.3f)\n", x, y
						}

						puts "};"
						printf "\\addlegendentry{%s}\n", lang
					}
				end
			end
			printf "\\caption{%s}", testcase
		end
	}

	size_result.each_with_index{|caseconts, i|
		latexenv "figure", opt: "h" do
			puts "\\centering"
			testcase, conts = caseconts

			latexenv "tikzpicture", opt: "scale = 0.45" do
				conts.each_with_index{|langsize, j|
					lang, size = langsize
					j = (j + (i * conts.length) + 1) * 3
					sizex = mapto_codesize size
					printf "\\draw[%s,fill] (%.2f, 0) rectangle (%.2f, %.2f);\n", lang, (j - 3), (j - 1), sizex
					printf "\\node[above] at (%.2f, %.2f) {%d};\n", (j - 2), sizex, size
				}
			end

			printf "\\caption{%s}", testcase
		end
	}
end
# }}}
# }}}

