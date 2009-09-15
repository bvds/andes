package andesdatashopcommunication;

import org.json.JSONException;
import org.json.JSONObject;

class Andes3ClientAction {

    /**
     * @return the command
     */
    public ClientCommands getCommand() {
        return command;
    }

    /**
     * @return the action
     */
    public JSONObject getAction() {
        return action;
    }
    public enum ClientCommands {

        OPEN_PROBLEM, SOLUTION_STEP, SEEK_HELP, CLOSE_PROBLEM
    }
    private ClientCommands command;
    private JSONObject action;

    public Andes3ClientAction(String action) throws JSONException {
        super();
        JSONObject givenAction = new JSONObject(action);
        /* dirty hack since JSON Objects use - for method names, which won't work
         * for java enum */
        this.command = ClientCommands.valueOf(givenAction.getString("method").replace('-', '_').toUpperCase());
        this.action = givenAction;
    }
}
