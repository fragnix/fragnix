# Steps to extract preprocessed module files from base, ghc-prim and integer-gmp
# delete non-source files and folders
find */ -type f -exec bash -c 'file=${1#./}; mv "$file" "${file//\//.}"' _ '{}' \;
# delete now-empty folders
rm *.hs-boot
rm *.hsc
ghc -E * -I../../../../base-4.9.1.0/include -optP -P -optL -P
rm *.hs
rename 's/.hspp$/.hs/' *.hspp
