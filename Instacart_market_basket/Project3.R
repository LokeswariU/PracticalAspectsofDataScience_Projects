library(arules)
#library(dplyr)
library(plyr)

#Fetching data from the three csv files uploaded in the Utdallas personal folder
departments <- read.csv("http://www.utdallas.edu/~lxu190000/departments.csv")
head(departments)
order_products <- read.csv("http://www.utdallas.edu/~lxu190000/order_products__prior.csv")
head(order_products)
products <- read.csv("http://www.utdallas.edu/~lxu190000/products.csv")
head(products)
class(products)

# Since there 3 files departments, order_products, products

#---------------------------------------- Order_products and Products Table Join ---------------------------------------------------------#

# Merging Order_products and the products files to get the join data of product-names for each orderId
order_products<-merge(order_products,products,by="product_id")
head(order_products)
# Can Use the ddply() to group and merge order_id and products
transactionData <- ddply(order_products,"order_id",function(df1)paste(df1$product_name,collapse = ","))

# Can use aggregate methoda also for grouping and merging the data
#agg <- aggregate(order_products$product_name , list(order_products$order_id), paste, collapse=",")


# Removing unwanted columns(order_id) of the merged table - transactionData
# Converting the data to class transaction and assigning items as column name
transactionData$order_id<-NULL
head(transactionData)
colnames(transactionData)<-c("items")

# Writing the data to the local system and uploading it in the personal UtDallas website
#write.csv(transactionData,"C:/Users/lokes/Desktop/Transaction_itemset.csv", quote = FALSE, row.names = FALSE)
#Since It is a huge data writing and reading it again is the best way to convert the data into transactions


#Reading the file from the personal UtDallas website 
transactionDataset <- read.transactions('http://www.utdallas.edu/~lxu190000/Transaction_itemset.csv', format = 'basket', sep=',')
 
#Summary and the sample data of the merged data converted to transactions
summary(transactionDataset)
inspect(head(transactionDataset))

#-------------------------------------Order_products and Departments Table Join------------------------------------------------------------------#

# Removing unwanted columns before merging the order_products table with departments to reduce the complexity
# Since we need only order_id and department for the join operation
order_products$add_to_cart_order<-NULL
order_products$reordered<-NULL
order_products$aisle_id<-NULL

# Merging Order_products and the department files to get the join data of department-names for each orderId
order_dept <- merge(order_products,departments,by="department_id")

# Using ddply() to group and merge order_id and department 
orderDataset <- ddply(order_dept,"order_id",function(df1)paste(df1$department,collapse = ","))

# Removing unwanted columns(order_id) of the merged table - orderDataset
# Converting the data to class transaction and assigning department as column name
orderDataset$order_id<-NULL
head(orderDataset)
colnames(orderDataset)<-"departments"

# Writing the data to the local system and uploading it in the personal UtDallas website
#write.csv(orderDataset,"C:/Users/lokes/Desktop/Order_department.csv", quote = FALSE, row.names = FALSE)

#Reading the file from the personal UtDallas website 
order_department <- read.transactions("http://www.utdallas.edu/~lxu190000/Order_department.csv", format = 'basket', sep=',')

#Summary and the sample data of the merged data converted to transactions
summary(order_department)
inspect(head(order_department))



#------------------------------------- Frequent Itemset for Products and Order_products ------------------------------------------------------------#

# Getting the frequent items with a minSupport of 0.01 and maxLength of 10
frequentItems <- eclat (transactionDataset,parameter = list(supp = 0.01 , maxlen = 10))
inspect(head(frequentItems,10))

#Plot for frequent itemsets
itemFrequencyPlot(transactionDataset, topN=15,type="absolute", main="Items Frequency")

#------------------------------------------- Association Rules for Products and Order_products-------------------------------------------------------#


#Association rules for products in transactionDataset dataset with a support = 0.01 and confidence = 0.5
item_rules <- apriori (transactionDataset,parameter = list(supp = 0.001, conf = 0.5))
summary(item_rules)
inspect(head(item_rules,10))

# Sorting the Association Rules of Items by Confidence, Support, Lift
confidence_item_rules <- sort (item_rules, by="confidence",decreasing=TRUE) 
support_item_rules <- sort (item_rules, by="support",decreasing=TRUE) 
lift_item_rules <- sort (item_rules, by="lift",decreasing=TRUE) 

# Resulting the support, lift and confidence for all item_rules
inspect(head(confidence_item_rules, 10))
inspect(head(support_item_rules,10))
inspect(head(lift_item_rules,10))

#---------------------------------------------- Frequent Itemset for Products and Order_products --------------------------------------------------------#

# Getting the frequent departments with a minSupport of 0.01 and maxLength of 10
frequentDepartments <- eclat (order_department,parameter = list(supp = 0.01, maxlen = 10))
inspect(head(frequentDepartments,10))

#Plot for frequent department sets
itemFrequencyPlot(order_department, topN=15,type="absolute", main="Department Frequency")

#-------------------------------------------------Association Rules for Products and Order_products ------------------------------------------------------#


#Association rules for departments in order_department dataset with a support = 0.01 and confidence = 0.5
department_rules <- apriori (order_department,parameter = list(supp = 0.01, conf = 0.5))
summary(department_rules)
inspect(head(department_rules,10))

# Sorting the Association Rules of Items by Confidence , Support, Lift
confidence_dept_rules <- sort (department_rules, by="confidence",decreasing=TRUE)
support_dept_rules <- sort (department_rules, by="support",decreasing=TRUE)
lift_dept_rules <- sort (department_rules, by="lift",decreasing=TRUE)

# Resulting the support, lift and confidence for all dept_rules
inspect(head(confidence_dept_rules, 10))
inspect(head(support_dept_rules, 10))
inspect(head(lift_dept_rules, 10))

