#!/bin/bash

# exit if a command exits with a non-zero status (aka failed)
set -e

# set constants
niter=500
duration=60
m=0
sup=1
maxage=13
slim_code="slim/code/111026_ch1_nuc.slim"
format_code="code/process_slim_vcf_v2.R"
calcgd_code="code/calc_gd_ch1_tick.R"

for i in $(seq 136 $niter); do
echo "====================================================="
echo "Starting iteration $i for wild m = ${m}"
echo "====================================================="

# run slim - specify migration rate
echo -e "\nRunning SLiM..."
slim -d sup=${sup} -d mig_rate=${m} -d iter=${i} -d duration=${duration} -d maxage=${maxage} ${slim_code}

# loop through output folder
for file in slim/output_vcf/ch1/sup*.vcf; do
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

# calculate Fst
echo -e "\nCalculating Fst..."
if [ -f "slim/files/ranch_fst.txt" ] && [ -f "slim/files/refuge_fst.txt" ]; then
if ! vcftools --vcf "slim/output_vcf/ch1/${filename}.vcf" --weir-fst-pop "slim/files/ranch_fst.txt" --weir-fst-pop "slim/files/refuge_fst.txt" --fst-window-size 150000 --fst-window-step 15000 --out "slim/output/ch1/gd/${filename}"; then
echo "====================== vcftools Fst failed ======================"
exit
fi
fi

# log results
echo -e "\nLogging results..."
Rscript ${calcgd_code} ${filename} ${i} ${m} ${duration} ${sup} ${maxage}

# remove file
rm slim/output_vcf/ch1/${filename}*
rm slim/output/ch1/gd/${filename}*
rm slim/files/*_fst.txt
done
done

echo -e "\nAll iterations finished\n"