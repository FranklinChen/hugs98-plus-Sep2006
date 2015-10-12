//
// The prototypical functional GUI application, the counter.
//
// To compile: csc /t:library ui.cs
//
using System;
using System.Windows.Forms;

public class UITest : Form {
 private Button upButton;
 private Button downButton;
 private Label  countLabel;
 private int count;
 
 public UITest() {
   upButton = new Button();
   upButton.Text = "Up";
   Controls.Add(upButton);

   downButton = new Button();
   downButton.Text = "Down";
   downButton.Location = new System.Drawing.Point (0,50);
   Controls.Add(downButton);
   
   countLabel = new Label();
   countLabel.Text = count.ToString();
   countLabel.Location = new System.Drawing.Point (0,30);
   countLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
   Controls.Add(countLabel);

   Text = "WinForms example";
   Height = 100;
   Width = 100;
 }
 
 public void IncCount() {
   count++;
   countLabel.Text = count.ToString();
 }
 
 public void DecCount() {
   count--;
   countLabel.Text = count.ToString();
 }
 
 public void AddHandlerUp(System.EventHandler h) {
   upButton.Click += h;
 }
 
 public void AddHandlerDown(System.EventHandler h) {
   downButton.Click += h;
 }
 
 public void RunIt() {
   Application.Run(this);
 }
 /*
 public void KickOff(System.EventHandler h) {
   button1.Click += h;
   Application.Run(this);
 }

 private void button1_click(object sender, EventArgs e) {
   MessageBox.Show("button1 clicked");
 }
 */

}
/*
public class TestApp {
 public static void Main(string[] args) {
   Application.Run(new SampleApp());
 }
}
*/
