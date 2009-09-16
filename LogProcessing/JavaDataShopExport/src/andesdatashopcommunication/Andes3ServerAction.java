package andesdatashopcommunication;

import org.json.JSONException;
import org.json.JSONObject;

class Andes3ServerAction {

    private JSONObject action;

    public Andes3ServerAction(String action) throws JSONException {
        super();
        this.action = new JSONObject(action);
    }

    /**
     * @return the action
     */
    public JSONObject getAction() {
        return action;
    }
}
