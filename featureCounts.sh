SAF="/mnt/localstorage/michelle/data/Collaborators/HudaZoghbi/Chloe_BigTau/MAYO_bams/BigTau.saf"
cerebellum="/mnt/localstorage/linhua/data/Mayo/cerebellum/MayoRNAseq_Cerebellum_BAMs/*.snap.bam"
tempCortex="/mnt/localstorage/linhua/data/Mayo/temporal_cortex/MayoRNAseq_Temporal_Cortex_BAMs/*.snap.bam"

featureCounts -T 20 -a $SAF -F SAF -s 0 -o temporalCortexCounts.txt $tempCortex





for bamfile in `ls /mnt/localstorage/linhua/data/Mayo/temporal_cortex/MayoRNAseq_Temporal_Cortex_BAMs/*.snap.bam`; do
	echo 'Processing:' $bamfile
	outName=$(echo $bamfile | rev | cut -d/ -f1 | rev |sed "s#.snap.bam#"_totalCounts.txt"#")
	samtools view -@ 20 -c -F 260 $bamfile > ./temporalCortex_totalCounts/$outName
done
