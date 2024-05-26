function sortedArray = quickSort(array)

    if numel(array) <= 1 %If the array has 1 element then it can't be sorted       
        sortedArray = array;
        return
    end
    
    pivot = array(end); % escolhe-se o ultimo como pivot
    array(end) = [];
        
    %Create two new arrays which contain the elements that are less than or
    %equal to the pivot called "less" and greater than the pivot called
    %"greater"
    less = array( array <= pivot ); % equivalente ao Cases[x, j_ /; j <= pivot] de mathematica 
    greater = array( array > pivot );
    
    %The sorted array is the concatenation of the sorted "less" array, the
    %pivot and the sorted "greater" array in that order
    % vai sendo chamado recursivamente e juntando as partes maior e menores
    % assim como nas outra linguagens
    sortedArray = [quickSort(less) pivot quickSort(greater)];
    
end