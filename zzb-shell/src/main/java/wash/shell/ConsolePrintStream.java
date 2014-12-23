package wash.shell;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-25
 * Time: 上午10:18
 */
public class ConsolePrintStream extends PrintStream {

    private ByteArrayOutputStream out = new ByteArrayOutputStream();

    public ConsolePrintStream(ByteArrayOutputStream out){
        super(out);
        this.out = out;
    }

    public String content(){
        return out.toString();
    }
}
