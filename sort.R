#selfsort
selfsort <- function(x){
        if(length(x)>1){
                min<-which.min(x)
c(x[min],selfsort(x[-min]))
}else x
}
dulieu <- c(32, 17, 49, 98, 06, 25, 53, 61)
selfsort(dulieu)


#sắp xếp chèn
insertion_sort <-function(x){
        n<-length(x)
        for(i in 2:(n)){
                temp<-x[i]
                j=i-1
        while((x[j]>temp)&& (j>=0)){
                x[j+1]  =x[j]
                j=j-1
        }
                x[j+1]=temp
                }
        return(x)
}


#bubble_sort
x<-sample(1:100,10)
example <- function(x){
        n<-length(x)
        for(j in 1:(n-1)){
                for(i in 1:(n-j)){
                        if(x[i]>x[i+1]){
                                temp<-x[i]
                                x[i]<-x[i+1]
                                x[i+1]<-temp
                        }
                }
        }
        return(x)
}


#bubble_sort2:

bubble_sort <- function(a){
        i=1
        repeat{
                no_exchange=1
                n = length(a)
                for (j in n:2){
                        if(a[j-1] > a[j]){
                                temp = a[(j-1)]
                                a[j] = temp
                                no_exchange = 0
                        }
                }
                i = i+1
                if (no_exchange || j == (n-1)) {break}
        }
        return(a)
}

#quick sort
> # Quick sort algorithm:
quickSort <- function(arr) {
mid <- sample(arr, 1)
left <- c()
right <- c()
# Move all the smaller values to the left, bigger values to the right.
lapply(arr[arr != mid], function(d) {
        if (d < mid) {
                left <<- c(left, d)
                }
        else {
                right <<- c(right, d)
                }
        })
                                
if (length(left) > 1) {
        left <- quickSort(left)
        }
                                
if (length(right) > 1) {
        right <- quickSort(right)
        }
                                
# Finally, return the sorted values.
c(left, mid, right)
}
x <-sample(1:100,10)
RES <- quickSort(x)
RES
 


#Heap sort
heap.building<-function(vec)
{
        len=length(vec)
        heap=vec
        for (j in len:1)
        {
                heap=modify.heap(heap,j)
        }
        return(heap)
}



is.heap<-function(heap,root_i)
{
        i=root_i
        res=T
        while(2*i<=length(heap)&res)
        {
                son=c(heap[2*i],heap[2*i+1])
                son=son[!is.na(son)]
                res=all(heap[i]<=son)
                i=i+1
        }
        return(res)
}


modify.heap<-function(heap,root_i)
{
        len=length(heap)
        flag=1
        
        while (root_i*2<=len&&flag==1)
        {
                left_i=root_i*2
                right_i=root_i*2+1
                flag=0
                son=c(heap[left_i],heap[right_i])
                son=son[!is.na(son)]
                min_ind=which.min(son)
                if (heap[root_i]>son[min_ind])
                {
                        flag=1
                        heap_ind=c(left_i,right_i)[min_ind]
                        
                        tmp=heap[heap_ind]
                        heap[heap_ind]=heap[root_i]
                        heap[root_i]=tmp
                        
                        root_i=heap_ind
                }
        }
        return(heap)
}

heap.sort<-function(heap)
{
        sorted=NULL
        len=length(heap)
        while(len>0)
        {
                sorted=c(sorted,heap[1])
                len=length(heap)
                heap[1]=heap[len]
                heap=heap[1:(len-1)]
                heap=modify.heap(heap,root_i=1)
                len=len-1
        }
        return(sorted)
}

vec=sample(1:100, 25, replace = TRUE)
vec
heap=heap.building(vec)
heap_sort=heap.sort(heap)
heap_sort
;

#Merger sort:
mmerge<-function(a,b) {
        r<-numeric(length(a)+length(b))
        ai<-1; bi<-1; j<-1;
        for(j in 1:length(r)) {
                if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
                        r[j] <- a[ai] 
                        ai <- ai+1
                        } else {
                                r[j] <- b[bi] 
                                bi <- bi+1
                        }
                }
        r
        }
mmergesort<-function(A) {
        if(length(A)>1) {
                q <- ceiling(length(A)/2)
                a <- mmergesort(A[1:q])
                b <- mmergesort(A[(q+1):length(A)])
                mmerge(a,b)
                } else {
                        A
                }
        }
x <-sample(1:100,10)

RES <- mmergesort(x)
RES
