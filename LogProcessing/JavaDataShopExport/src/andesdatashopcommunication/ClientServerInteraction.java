/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package andesdatashopcommunication;

import java.util.List;
import org.json.JSONException;
import org.json.JSONObject;

/**
 *
 * @author Andes Version 3 Tutoring System
 */
public class ClientServerInteraction {

    private Andes3ClientAction clientAction;
    private List<Andes3ServerAction> serverActions;

    /**
     * @return the clientAction
     */
    public Andes3ClientAction getClientAction() {
        return clientAction;
    }

    /**
     * @param clientAction the clientAction to set
     */
    public void setClientAction(Andes3ClientAction clientAction) {
        this.clientAction = clientAction;
    }

    /**
     * @return the serverActions
     */
    public List<Andes3ServerAction> getServerActions() {
        return serverActions;
    }

    /**
     * @param serverActions the serverActions to set
     */
    public void setServerActions(List<Andes3ServerAction> serverActions) {
        this.serverActions = serverActions;
    }

    @Override
    public String toString() {
        String serverStuff = "";
        for (Andes3ServerAction coms : getServerActions()) {
            serverStuff += coms.getAction() + "\n";
        }
        return String.format("%nThe client command is %s%nThe server commands are %s%n",
                getClientAction().getAction(), serverStuff);
    }

    public boolean wasCorrectClientAction() throws JSONException {
        boolean returnValue = false;
        for (Andes3ServerAction anAction : getServerActions()) {
            if (anAction.getAction().has("action") &&
                    anAction.getAction().getString("action").equals("modify-object")) {
                if (anAction.getAction().has("mode") &&
                        anAction.getAction().getString("mode").equals("correct")) {
                    returnValue = true;
                } else if (anAction.getAction().has("mode") &&
                        anAction.getAction().getString("mode").equals("incorrect")) {
                    returnValue = false;
                }
            }
        }
        return returnValue;
    }

    public String getClientSelection() throws JSONException {
        String returnValue = new String();
        returnValue = (getClientAction().getAction().has("params") &&
                getClientAction().getAction().getJSONObject("params").has("id")) ? getClientAction().getAction().getJSONObject("params").getString("id")
                : "";
        return returnValue;
    }

    public String getClientInput() {
        return getClientAction().getAction().toString();
    }

    public boolean isServerHelp() throws JSONException {
        if (getClientAction().getCommand() == Andes3ClientAction.ClientCommands.SEEK_HELP) {
            for (Andes3ServerAction act : getServerActions()) {
                if (act.getAction().has("action") &&
                        act.getAction().getString("action").equals("show-hint")) {
                    return true;
                }
            }
        }

        return false;
    }

    public String getServerSelection() throws JSONException {
        String returnValue = new String();
        for (Andes3ServerAction act : getServerActions()) {
            if (act.getAction().has("id")) {
                returnValue = act.getAction().getString("id");
            }
        }
        return returnValue;
    }

    public String getServerAction() throws JSONException {
        return getServerInput();
    }

    public String getServerInput() throws JSONException {
        String returnValue = new String();
        for (Andes3ServerAction act : getServerActions()) {
            if (act.getAction().has("assoc")) {
                returnValue = act.getAction().getJSONObject("assoc").getString(JSONObject.getNames(act.getAction().getJSONObject("assoc"))[0]);
            } else if (act.getAction().has("text") && act.getAction().getString("action").equals("show-hint")) {
                returnValue = act.getAction().getString("text");
            }
        }
        return returnValue;
    }

    public String getSkill() throws JSONException {
        String returnValue = new String();
        if (!isServerHelp()) {
            for (Andes3ServerAction act : getServerActions()) {
                if (act.getAction().has("assoc")) {
                    returnValue =
                            JSONObject.getNames(act.getAction().getJSONObject("assoc"))[0];
                }
            }
        }

        return returnValue;
    }

    public String getScore() throws JSONException {
        String returnValue = new String();
        if (hasSetScore()) {
            for (Andes3ServerAction act : getServerActions()) {
                if (act.getAction().has("action") &&
                        act.getAction().getString("action").equals("set-score") &&
                        act.getAction().has("score")) {
                    returnValue = act.getAction().getString("score");
                }
            }
        }

        return returnValue;
    }

    public boolean hasSetScore() throws JSONException {
        for (Andes3ServerAction act : getServerActions()) {
            if (act.getAction().has("action") &&
                    act.getAction().getString("action").equals("set-score") &&
                    act.getAction().has("score")) {
                return true;
            }
        }
        return false;
    }
}
