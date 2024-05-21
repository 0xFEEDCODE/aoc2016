using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace day11Visualizer;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow : Window
{
    private VisualizerViewModel _viewModel;
    
    public MainWindow()
    {
        InitializeComponent();
        _viewModel = new VisualizerViewModel(ref Canvas, ref Slider);
        DataContext = _viewModel;
    }

    private bool AutoScroll = true;

    private void ScrollViewer_ScrollChanged(object sender, ScrollChangedEventArgs e)
    {
        var scrollViewer = (ScrollViewer)sender;
        if (e.ExtentHeightChange == 0)
        {
            AutoScroll = scrollViewer.VerticalOffset == scrollViewer.ScrollableHeight; // Scroll bar isn't in bottom
        }

        if (AutoScroll && e.ExtentHeightChange != 0)
            scrollViewer.ScrollToVerticalOffset(scrollViewer.ExtentHeight);
    }
}