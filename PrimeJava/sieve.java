import java.util.Scanner;
public class sieve{
    static boolean isPrime(int number){
        boolean is=true;
        for(int x=2;x<number;x++){
            if(number%x==0){
                is=false;
                break;
            }
        }
        return is;
    }
    public static void main(String[] args){
     Scanner input=new Scanner(System.in);
        System.out.println("Enter a number:");
        int num=input.nextInt();
        for(int x=2;x<=num;x++){
            if(isPrime(x)){
                System.out.println(x);
            }
        }
    }
}