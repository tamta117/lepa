#!/bin/bash

# exit if a command exits with a non-zero status (aka failed)
set -e

# set constants
niter=500
duration=60
m=2
sup=1
f=75
sample_code="code/sample_ch2.R"
founder_gd="code/calc_gd_founders.R"
slim_code="slim/code/101526_ch2"
format_code="code/process_slim_vcf_ch2.R"
calcgd_code="code/calc_gd_ch2.R"
# rscript="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"
# plink1="/c/Users/kutlt017/Documents/plink_win64_20250819/plink.exe"
# plink2="/c/Users/kutlt017/Documents/plink2_win64_20260504/plink2.exe"

for i in $(seq 1 $niter); do
echo "====================================================="
echo "Iteration $i for ch2 m = ${m} sup = ${sup} f = ${f}"
echo "====================================================="

# sample founders and supplements
echo -e "\nSampling founders and supplements..."
Rscript ${sample_code} ${m} ${sup} ${f}
# calculate pi
echo -e "\nCalculating pi for founders..."
if ! vcftools --vcf "slim/input_vcf/ch2/founders.vcf" --window-pi 150000 --out "slim/output/ch2/gd/founders" 2>/dev/null; then
echo "====================== vcftools pi failed ======================"
exit
fi

# log results
echo -e "\nLogging results for founders..."
Rscript ${founder_gd} ${i} ${m} ${sup} ${f}

# run slim - specify migration rate
echo -e "\nRunning SLiM..."
slim -d iter=${i} -d mig_rate=${m} -d sup=${sup} ${slim_code}

# continue to next iteration if extinct
if [ ! -f "slim/output_vcf/ch2/sim11.vcf" ]; then
echo -e "\nBad seed, starting next iteration..."
continue
fi

# process outputs
for t in $(seq 11 10 61); do
file="sim${t}"

# break rest of the loop if file don't exist
if [ ! -f "slim/output_vcf/ch2/${file}.vcf" ]; then
echo -e "\nNo ${file}, starting next iteration..."
break
fi

# extract samples and gt
echo -e "\nExtract samples and genotype..."
bcftools query -f '%CHROM\t%POS\t%REF\t%ALT[\t%GT]\n' "slim/output_vcf/ch2/${file}.vcf" > "slim/output_vcf/ch2/${file}_gt.txt"
bcftools query -l "slim/output_vcf/ch2/${file}.vcf" > "slim/output_vcf/ch2/${file}_samples.txt"

# format output VCF
echo -e "\nStitching VCFs..."
Rscript ${format_code} ${file}

# calculate het
echo -e "\nCalculating heterozygosity..."
if ! plink2 --bfile "slim/output_vcf/ch2/${file}" --silent --freq --out "slim/output/ch2/gd/${file}"; then
echo "====================== plink freq failed ======================"
exit
fi
if ! plink2 --bfile "slim/output_vcf/ch2/${file}" --silent --het --read-freq "slim/output/ch2/gd/${file}.afreq" --out "slim/output/ch2/gd/${file}"; then
echo "====================== plink het failed ======================"
exit
fi

# calculate pi
echo -e "\nCalculating pi..."
if ! vcftools --vcf "slim/output_vcf/ch2/${file}.vcf" --window-pi 150000 --out "slim/output/ch2/gd/${file}" 2>/dev/null; then
echo "====================== vcftools pi failed ======================"
exit
fi

# calculate ROH
echo -e "\nCalculating ROH..."
if ! bcftools roh -G30 -e - --rec-rate 1.1e-8 "slim/output_vcf/ch2/${file}.vcf" -Or -o "slim/output/ch2/gd/${file}_roh.txt"; then
echo "====================== bcftools ROH failed ======================"
exit
fi

# log results
echo -e "\nLogging results..."
Rscript ${calcgd_code} ${file} ${i} ${m} ${sup} ${duration} ${f}
# remove file
rm "slim/output_vcf/ch2/${file}.vcf"
done
done

echo -e "\nAll iterations finished\n"