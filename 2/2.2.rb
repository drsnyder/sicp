
def permutations(seq)
    if seq.empty? || seq.count == 1
        seq
    else
        seq.map { |x|
            permutations(seq.select { |e| e != x }).map { |p|
                if p.class == Fixnum
                    [x, p]
                else
                    p.unshift(x)
                end
            }
        }.flatten(1)
    end
end
permutations [1]
permutations [1,2]
permutations [1,2,3]

# compare to [1,2,3].permutations(3)
