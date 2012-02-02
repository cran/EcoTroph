check.table <-
function(ecopath)
{

##check format
if(is.data.frame(ecopath)==FALSE)
ecopath <- as.data.frame(ecopath)
	

##check column names

names <- names(ecopath)
wanted <- c("group_name","TL","biomass","prod","accessibility","OI")
verif <- is.na(match(wanted, names))
pb<-paste(wanted[which(verif==TRUE)],collapse=" ")
if (pb!='') cat(paste("The column(s) ",pb," is(are) not present.(if the column OI is not present, it's not a problem, the use of the OI smooth is just not possible)\n"))


#Check for 'catch*'
if (length(grep("catch",names))==0) print ("No fleet catches are detected. Even if no catches are maid (MPA or no data), a column with 0 value must be entered.")


##remplacement des NA

cat("
CHECK OF THE NA VALUES, IF A MESSAGE APPEARS, PLEASE READ IT CAREFULLY AND FOLLOW THE INSTRUCTIONS.
\n")

for (i in wanted[-match(wanted[verif],wanted)])
{
if(match(NA,ecopath[,i],FALSE)>0)
{
if (i=="prod")
{
pb<-paste(ecopath[is.na(ecopath$prod),]$group_name,collapse=" ")
cat(paste("the column 'prod' contains NA for ",pb,", that's a problem if the groups concerned are not detritus. Use fix() to change the NA value by 0 if the concerned group is Detritus, otherwhise check your table and put also the right value (use fix() for example or reload it after correction)\n"))
}
else
{
toto <- paste(i)
cat(paste("the column",i,"  contains a NA, that's a problem! Check and correct the table (you can use fix())\n"))
}}}

for (pecheries in colnames(ecopath)[grep ("catch",colnames(ecopath))])
{
if(match(NA,ecopath[,pecheries],FALSE)>0)
{
cat(paste("the column",pecheries,"contains NA. That's a problem! Even if no catches are maid (MPA or no data), 0 value must be entered. Use fix() to change the NA by the proper value or reload the dataset after correction.\n"))
}}

}