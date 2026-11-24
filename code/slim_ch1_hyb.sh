#!/bin/bash

# exit if a command exits with a non-zero status (aka failed)
set -e

# set constants
niter=500
m=2
sup=1
f=50
pop="ranch"
sample_code="code/sample_ch1_hyb.R"
slim_code="slim/code/112026_ch1_hyb.slim"
format_code="code/process_slim_vcf_hyb.R"
calcgd_code="code/calc_gd_ch1_hyb.R"

for i in $(seq 91 $niter); do
echo "====================================================="
echo "Starting iteration $i for ch1 ${f}% m = ${m}"
echo "====================================================="

# sample hyb
Rscript ${sample_code} ${m} ${sup} ${f}

# run slim - specify migration rate
echo -e "\nRunning SLiM..."
slim -d sup=${sup} -d mig_rate=${m} -d iter=${i} -d f=${f} -d "pop='${pop}'" ${slim_code}

# loop through output folder
for file in slim/output_vcf/ch1/hyb*.vcf; do
filename=$(basename "$file" .vcf)

# extract samples and gt
echo -e "\nExtract samples and genotype..."
bcftools query -f '%CHROM\t%POS\t%REF\t%ALT[\t%GT]\n' "slim/output_vcf/ch1/${filename}.vcf" > "slim/output_vcf/ch1/${filename}_gt.txt"
bcftools query -l "slim/output_vcf/ch1/${filename}.vcf" > "slim/output_vcf/ch1/${filename}_samples.txt"

# format output VCF
echo -e "\nStitching VCFs..."
Rscript ${format_code} ${filename}

# calculate het
echo -e "\nCalculating heterozygosity..."
if ! plink2 --bfile "slim/output_vcf/ch1/${filename}" --silent --freq --out "slim/output/ch1/gd/${filename}"; then
echo "====================== plink freq failed ======================"
exit
fi
if ! plink2 --bfile "slim/output_vcf/ch1/${filename}" --silent --het --read-freq "slim/output/ch1/gd/${filename}.afreq" --out "slim/output/ch1/gd/${filename}"; then
echo "====================== plink het failed ======================"
exit
fi

# calculate ROH
echo -e "\nCalculating ROH..."
if ! bcftools roh -G30 -e - --rec-rate 1.1e-8 "slim/output_vcf/ch1/${filename}.vcf" -Or -o "slim/output/ch1/gd/${filename}_roh.txt"; then
echo "====================== bcftools ROH failed ======================"
exit
fi

# log results
echo -e "\nLogging results..."
Rscript ${calcgd_code} ${filename} ${i} ${m} ${sup} ${f} ${pop}

# remove file
rm slim/output_vcf/ch1/${filename}*
rm slim/output/ch1/gd/${filename}*
done
done

echo -e "\nAll iterations finished\n"