import java.util.Scanner;

public class Runtime {

    private static Scanner scan = new Scanner(System.in);

    public static void writeInt (int n) {
        System.out.println(n);
    }

    public static void writeReal (double x) {
        System.out.println(String.format( "%.1f", x ));
    }

    public static void writeStr (String s) {
        System.out.println(s);
    }

    public static int readInt () {
        return scan.nextInt();
    }

    public static double readReal () {
        return scan.nextDouble();
    }

    // Para testear and cortocicuitado
    public static int writeIntBool (int n) {
        System.out.println(n);
	return 1;
    }
}
