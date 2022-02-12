/* Example
 * Author: 
 * Creation date: 2021-12-22
 */
MACHINE
    Example
    
CONSTANTS
    Ladders,
    Snakes,
    Squares,
    Start,
    End
    
PROPERTIES
    Squares <: NAT1 & Squares = 1..100 &
    Snakes<: NAT1 & Snakes = {16, 31, 47, 63, 66, 97} &
    Ladders<: NAT1 & Ladders = {3, 10, 27, 56, 61, 72} &
    Start: NAT1 & Start = 1 &
    End: NAT1 & End = 100
    

END
