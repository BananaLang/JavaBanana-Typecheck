package banana.nullstubs.java.lang;

import java.lang.invoke.MethodHandles;
import java.nio.charset.Charset;
import java.util.Comparator;
import java.util.Locale;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import banana.internal.annotation.NonNull;
import banana.internal.annotation.Nullable;

abstract class StringStubs {
    @NonNull
    public static final Comparator<String> CASE_INSENSITIVE_ORDER = null;

    public StringStubs() {}
    public StringStubs(@NonNull String original) {}
    public StringStubs(@NonNull char[] value) {}
    public StringStubs(@NonNull char[] value, int offset, int count) {}
    public StringStubs(@NonNull int[] codePoints, int offset, int count) {}
    public StringStubs(@NonNull byte[] ascii, int hibyte, int offset, int count) {}
    public StringStubs(@NonNull byte ascii[], int hibyte) {}
    public StringStubs(@NonNull byte[] bytes, int offset, int length, @NonNull String charsetName) {}
    public StringStubs(@NonNull byte[] bytes, int offset, int length, @NonNull Charset charset) {}
    public StringStubs(@NonNull byte[] bytes, @NonNull String charsetName) {}
    public StringStubs(@NonNull byte[] bytes, @NonNull Charset charset) {}
    public StringStubs(@NonNull byte[] bytes, int offset, int length) {}
    public StringStubs(@NonNull byte[] bytes) {}
    public StringStubs(@NonNull StringBuffer buffer) {}
    public StringStubs(@NonNull StringBuilder builder) {}

    public abstract void getChars(int srcBegin, int srcEnd, @NonNull char[] dst, int dstBegin);
    public abstract void getBytes(int srcBegin, int srcEnd, @NonNull byte[] dst, int dstBegin);
    @NonNull
    public abstract byte[] getBytes(@NonNull String charsetName);
    @NonNull
    public abstract byte[] getBytes(@NonNull Charset charset);
    @NonNull
    public abstract byte[] getBytes();
    public abstract boolean equals(@Nullable Object anObject);
    public abstract boolean contentEquals(@NonNull StringBuffer sb);
    public abstract boolean contentEquals(@NonNull CharSequence cs);
    public abstract boolean equalsIgnoreCase(@Nullable String anotherString);
    public abstract int compareTo(@NonNull String anotherString);
    public abstract int compareToIgnoreCase(@NonNull String str);
    public abstract boolean regionMatches(int toffset, @NonNull String other, int ooffset, int len);
    public abstract boolean regionMatches(boolean ignoreCase, int toffset,
            @NonNull String other, int ooffset, int len);
    public abstract boolean startsWith(@NonNull String prefix, int toffset);
    public abstract boolean startsWith(@NonNull String prefix);
    public abstract boolean endsWith(@NonNull String suffix);
    public abstract int indexOf(@NonNull String str);
    public abstract int indexOf(@NonNull String str, int fromIndex);
    public abstract int lastIndexOf(@NonNull String str);
    public abstract int lastIndexOf(@NonNull String str, int fromIndex);
    @NonNull
    public abstract String substring(int beginIndex);
    @NonNull
    public abstract String substring(int beginIndex, int endIndex);
    @NonNull
    public abstract CharSequence subSequence(int beginIndex, int endIndex);
    @NonNull
    public abstract String concat(@NonNull String str);
    @NonNull
    public abstract String replace(char oldChar, char newChar);
    public abstract boolean matches(@NonNull String regex);
    public abstract boolean contains(@NonNull CharSequence s);
    @NonNull
    public abstract String replaceFirst(@NonNull String regex, @NonNull String replacement);
    @NonNull
    public abstract String replaceAll(@NonNull String regex, @NonNull String replacement);
    @NonNull
    public abstract String replace(@NonNull CharSequence target, @NonNull CharSequence replacement);
    @NonNull
    public abstract String[] split(@NonNull String regex, int limit);
    @NonNull
    public abstract String[] split(@NonNull String regex);
    @NonNull
    public abstract String toLowerCase(@NonNull Locale locale);
    @NonNull
    public abstract String toLowerCase();
    @NonNull
    public abstract String toUpperCase(@NonNull Locale locale);
    @NonNull
    public abstract String toUpperCase();
    @NonNull
    public abstract String trim();
    @NonNull
    public abstract String strip();
    @NonNull
    public abstract String stripLeading();
    @NonNull
    public abstract String stripTrailing();
    @NonNull
    public abstract Stream<String> lines();
    @NonNull
    public abstract String indent(int n);
    @NonNull
    public abstract String stripIndent();
    @NonNull
    public abstract String translateEscapes();
    @NonNull
    public abstract String toString();
    @NonNull
    public abstract IntStream chars();
    @NonNull
    public abstract IntStream codePoints();
    @NonNull
    public abstract char[] toCharArray();
    @NonNull
    public abstract String formatted(@Nullable Object... args);
    @NonNull
    public abstract String intern();
    @NonNull
    public abstract String repeat(int count);
    @NonNull
    public abstract Optional<String> describeConstable();
    @NonNull
    public abstract String resolveConstantDesc(@Nullable MethodHandles.Lookup lookup);

    @NonNull
    public static String join(@NonNull CharSequence delimiter, @Nullable CharSequence... elements) { return null; }
    @NonNull
    public static String join(@NonNull CharSequence delimiter,
            @NonNull Iterable<? extends CharSequence> elements) { return null; }
    @NonNull
    public static String format(@NonNull String format, @Nullable Object... args) { return null; }
    @NonNull
    public static String format(Locale l, String format, Object... args) { return null; }
    @NonNull
    public static String valueOf(@Nullable Object obj) { return null; }
    @NonNull
    public static String valueOf(@NonNull char[] data) { return null; }
    @NonNull
    public static String valueOf(@NonNull char[] data, int offset, int count) { return null; }
    @NonNull
    public static String copyValueOf(@NonNull char[] data, int offset, int count) { return null; }
    @NonNull
    public static String copyValueOf(@NonNull char[] data) { return null; }
    @NonNull
    public static String valueOf(boolean b) { return null; }
    @NonNull
    public static String valueOf(char c) { return null; }
    @NonNull
    public static String valueOf(int i) { return null; }
    @NonNull
    public static String valueOf(long l) { return null; }
    @NonNull
    public static String valueOf(double d) { return null; }
}
