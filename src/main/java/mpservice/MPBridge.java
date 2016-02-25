package mpservice;

import java.util.LinkedList;
import java.util.List;
import java.util.Arrays;
import mpservice.ModPow;
import java.math.BigInteger;
import java.util.function.Supplier;
import com.squareup.jnagmp.Gmp;

public class MPBridge {
	public static long total = 0;
	public static long found = 0;
	public static long before = 0;
	public static long beforeZ = 0;
	public static long foundZ = 0;
	public static boolean debug = false;
	public static boolean gmpModPow = false;

	public static BigInteger dummy = new BigInteger("2");
	
	public static BigInteger modulus = null;
	private static long extracted = 0;
	private static boolean recording = false;
	private static boolean replaying = false;
	private static LinkedList<ModPow2> requests = new LinkedList<ModPow2>();
	private static List<BigInteger> answers = null;

	public static void l() {
		StackTraceElement[] traces = Thread.currentThread().getStackTrace();
		StackTraceElement caller = traces[2];
		System.err.println("* " + caller.getFileName() + ":" + caller.getLineNumber() + "..");	
	}

	public static void a() {
		before = total;
	}

	public static void b(int trace) {
		long diff = total - before;
		StackTraceElement[] traces = Thread.currentThread().getStackTrace();
		StackTraceElement caller = traces[trace];
		found += diff;
		System.err.println(">>> " + caller.getFileName() + ":" + caller.getLineNumber() + " [" + diff + "]" + " (" + found + ", " + total + ") (" + extracted + ")");	
	}

	public static void b() {
		b(3);
	}

	public static void y() {
		beforeZ = total;
		foundZ = found;
	}

	public static void z() {
		long diff = total - beforeZ;
		long diffFound = found - foundZ;
		StackTraceElement[] traces = Thread.currentThread().getStackTrace();
		StackTraceElement caller = traces[2];
		System.err.println("> " + caller.getFileName() + ":" + caller.getLineNumber() + " [" + diff + "]" + " (" + found + ", " + diffFound + ", " + total + ") (" + extracted + ")");		
	}

	public static void startRecord() {
		startRecord("2");
	}

	public static void startRecord(String value) {
		dummy = new BigInteger(value);
		if(requests.size() != 0)	throw new IllegalStateException();
		recording = true;
		modulus = null;
	}

	public static ModPow2[] stopRecord() {
		recording = false;

		return requests.toArray(new ModPow2[0]);
	}

	public static boolean isRecording() {
		return recording;
	}

	public static synchronized void addModPow(BigInteger base, BigInteger pow, BigInteger mod) {
		if(!recording) throw new IllegalStateException();
		if(modulus == null) {
			modulus = mod;
		}
		else if(!modulus.equals(mod)) {
			throw new RuntimeException(modulus + "!=" + mod);
		}
		extracted++;
		requests.add(new ModPow2(base, pow));
	}

	public static BigInteger getModPow() {
		if(recording) throw new IllegalStateException();

		return answers.remove(0);
	}

	public static LinkedList<ModPow2> getRequests() {
		if(recording) throw new IllegalStateException();

		return requests;
	}

	public static long getExtracted() {
		return extracted;
	}

	public static void startReplay(BigInteger[] answers_) {
		if(answers_.length != requests.size()) throw new IllegalArgumentException();	
		answers = new LinkedList<BigInteger>(Arrays.asList(answers_));

		replaying = true;
	}

	public static void stopReplay() {
		if(answers.size() != 0) throw new IllegalStateException();

		replaying = false;
	}

	public static void reset() {
		requests.clear();
	}

	public static boolean isReplaying() {
		return replaying;
	}

	public static <T> T ex(Supplier<T> f, String v) {
		a();
	 	startRecord(v);
	 	T ret = f.get();
	 	mpservice.ModPow2[] requests = MPBridge.stopRecord();
		b(3);
		if(requests.length > 0) {
			java.math.BigInteger[] answers = mpservice.MPService.compute(requests, modulus);
			MPBridge.startReplay(answers);
			ret = f.get();	
			MPBridge.stopReplay();
		}
		MPBridge.reset();

		return ret;
	}

	public static <T> T ex(Supplier<T> f) {
		return ex(f, "2");
	}

	public static synchronized BigInteger modPow(BigInteger base, BigInteger pow, BigInteger mod) {
        if(MPBridge.debug) new Exception().printStackTrace();
        if(MPBridge.isRecording()) {
            MPBridge.total++;
            MPBridge.addModPow(base, pow, mod);
            return MPBridge.dummy;
        }
        else if(MPBridge.isReplaying()) {
            return MPBridge.getModPow();
        }
        else {
            MPBridge.total++;
            if(gmpModPow) {
                return Gmp.modPowInsecure(base, pow, mod);
            }
            else {
                return base.modPow(pow, mod);    
            }
        }
    }
}